port module PhotoGroove exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Random exposing (..)


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : Decoder Photo
photoDecoder =
    Decode.succeed (\u s t -> { url = u, size = s, title = t })
        |> Pipeline.required "url" Decode.string
        |> Pipeline.required "size" Decode.int
        |> Pipeline.optional "title" Decode.string "(untitled)"


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Model =
    { chosenSize : ThumbnailSize
    , status : Status
    , activity : String
    , hue : Int
    , ripple : Int
    , noise : Int
    }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


initialModel : Model
initialModel =
    { chosenSize = Medium
    , status = Loading
    , activity = ""
    , hue = 2
    , ripple = 2
    , noise = 2
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Decode.list photoDecoder)
        }


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser selectedSize size =
    label []
        [ input
            ([ type_ "radio"
             , name "size"
             , onClick (ChooseSize size)
             ]
                ++ (if selectedSize == size then
                        [ checked True ]

                    else
                        []
                   )
            )
            []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src ("http://elm-in-action.com/" ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickPhoto thumb.url)
        ]
        []


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider =
    node "range-slider"



onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    Decode.at [ "detail", "userSlidTo" ] Decode.int
        |> Decode.map toMsg
        |> on "slide"


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attrs.max "11"
            , property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/large/"


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ h1 [] [ text "Photo Grove" ]
    , button
        [ onClick ClickSuprise ]
        [ text "Suprise me!" ]
    , div [ class "activity" ] [ text model.activity ]
    , div [ class "filters" ]
        [ viewFilter GotSlidHue "Hue" model.hue
        , viewFilter GotSlidRipple "Ripple" model.ripple
        , viewFilter GotSlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumbnail Size" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , canvas
        [ id "main-canvas"
        , class "large"
        ]
        []
    ]


type Msg
    = ClickPhoto String
    | ClickSuprise
    | ChooseSize ThumbnailSize
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | GotSlidHue Int
    | GotSlidRipple Int
    | GotSlidNoise Int
    | GotActivity String


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        _ ->
            status


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickPhoto img ->
            applyFilters { model | status = selectUrl img model.status }

        ClickSuprise ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                _ ->
                    ( model, Cmd.none )

        ChooseSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        GotPhotos (Ok ((first :: _) as photos)) ->
            applyFilters { model | status = Loaded photos first.url }

        GotPhotos (Ok []) ->
            ( { model | status = Errored "Server didn't return any photos?" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        GotSlidHue amount ->
            applyFilters { model | hue = amount }

        GotSlidRipple amount ->
            applyFilters { model | ripple = amount }

        GotSlidNoise amount ->
            applyFilters { model | noise = amount }

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (String -> msg) -> Sub msg


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded _ selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ selectedUrl
            in
            ( { model | status = selectUrl selectedUrl model.status }, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )


type alias FilterOptions =
    { url : String
    , filters :
        List
            { name : String
            , amount : Float
            }
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    activityChanges GotActivity


init : Float -> ( Model, Cmd Msg )
init version =
    let
        activity =
            "Initializing Pasta v" ++ String.fromFloat version
    in
    ( { initialModel | activity = activity }, initialCmd )


main : Program Float Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
