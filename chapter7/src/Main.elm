module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)


type alias Model =
    { page : Page }


type Page
    = SelectedPhoto String
    | Gallery
    | Folders
    | NotFound


view : Model -> Document Msg
view model =
    { title = "PhotoGroove Single Page App"
    , body =
        [ lazy viewHeader model.page
        , text "This isn't even my final form"
        , lazy (\_ -> viewFooter) ()
        ]
    }


show : Bool -> String
show b =
    if b then
        "True"

    else
        "False"


viewFooter : Html msg
viewFooter =
    footer [] [ text "One is never alone with a rubber duck. -Douglas Adams" ]


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Photo Groove" ]

        links =
            ul []
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]

        navLink : Page -> { url : String, caption : String } -> Html msg
        navLink targetPage { url, caption } =
            case targetPage of
                Gallery ->
                  let
                      isGallery = show (page == targetPage)
                      message = "Should be true: " ++ isGallery
                  in
                    Debug.log message
                        li
                        [ classList [ ( "active", page == targetPage ) ] ]
                        [ a [ href url ] [ text caption ] ]

                _ ->
                    li [ classList [ ( "active", page == targetPage ) ] ] [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url


parser : Parser (Page -> a) a
parser =
    Parser.map SelectedPhoto (s "photos" </> Parser.string)


urlToPage : Url -> Page
urlToPage url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUrl url ->
            Debug.log ("Path is: " ++ url.path)
              ( { model | page = urlToPage url }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case url.path of
        "/gallery" ->
            ( { page = Gallery }, Cmd.none )

        "/" ->
            ( { page = Folders }, Cmd.none )

        _ ->
            ( { page = NotFound }, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = \_ _ _ -> ( { page = Folders }, Cmd.none )
        , onUrlRequest = \_ -> Debug.todo "handle URL requests"
        , onUrlChange = ChangedUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
