module PhotoGroove exposing (main)

-- Elm Runtime (ER)
-- Msg -> update -> Model -> view -> Html
-- ER produces a Msg which triggers the `update`
-- function. `update` modifies the `Model`
-- `view` takes the Model and produces Html
-- Handling Events
-- ---
-- Click Event -> Elm Runtime -> Msg -> update
-- Interpret Msg, update view and model

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random exposing (Generator)


type alias Photo =
    { url : String }


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }


randomPhotoPicker : Generator Int
randomPhotoPicker =
    Random.int 0 (List.length initialModel.photos - 1)


getPhotoUrl : Int -> List Photo -> String
getPhotoUrl idx ps =
    case Array.get idx (Array.fromList ps) of
        Just x ->
            x.url

        Nothing ->
            "1.jpeg"


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
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
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickPhoto thumb.url)
        ]
        []


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Grove" ]
        , button
            [ onClick ClickSuprise ]
            [ text "Suprise me!" ]
        , h3 [] [ text "Thumbnail Size" ]
        , div [ id "choose-size" ]
            (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src ("http://elm-in-action.com/large/" ++ model.selectedUrl)
            ]
            []
        ]


type Msg
    = ClickPhoto String
    | ClickSuprise
    | ChooseSize ThumbnailSize
    | GotSelectedIndex Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickPhoto img ->
            ( { model | selectedUrl = img }, Cmd.none )

        ClickSuprise ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

        ChooseSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotSelectedIndex idx ->
            ( { model | selectedUrl = getPhotoUrl idx model.photos }, Cmd.none )



-- main : Html Model

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
