module PhotoGroove exposing (main)

import Browser
import Dict exposing (toList)
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
    , hue : Int
    , ripple : Int
    , noise : Int
    }

-- This models a single HTTP request
-- to a server for many images


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


initialModel : Model
initialModel =
    { chosenSize = Medium
    , status = Loading
    , hue = 5
    , ripple = 5
    , noise = 5
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



-- {detail: {userSlidTo: <int>}}


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


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ h1 [] [ text "Photo Grove" ]
    , button
        [ onClick ClickSuprise ]
        [ text "Suprise me!" ]
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
    , img
        [ class "large"
        , src ("http://elm-in-action.com/large/" ++ selectedUrl)
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
            ( { model | status = selectUrl img model.status }, Cmd.none )

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
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        GotPhotos (Ok ((first :: _) as photos)) ->
            ( { model | status = Loaded photos first.url }, Cmd.none )

        GotPhotos (Ok []) ->
            ( { model | status = Errored "Server didn't return any photos?" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        GotSlidHue amount ->
            ( { model | hue = amount }, Cmd.none )

        GotSlidRipple amount ->
            ( { model | ripple = amount }, Cmd.none )

        GotSlidNoise amount ->
            ( { model | noise = amount }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
