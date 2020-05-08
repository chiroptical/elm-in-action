module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr exposing (src)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode exposing (int, object, string)
import PhotoGroove exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    test "one plus one equals two" (\_ -> Expect.equal 2 (1 + 1))



-- decoderTest : Test
-- decoderTest =
--     test "title defaults to '(untitled)'" <|
--         \_ ->
--             """
--             {
--               "url": "fruits.com",
--               "size": 5
--             }
--             """
--                 |> decodeString PhotoGroove.photoDecoder
--                 |> Result.map .title
--                 |> Expect.equal (Ok "(untitled)")


defaultTitleDecoder : Test
defaultTitleDecoder =
    fuzz2 Fuzz.string Fuzz.int "title defaults to '(untitled)'" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> Json.Decode.decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")


titleDecoder : Test
titleDecoder =
    fuzz3 Fuzz.string Fuzz.int Fuzz.string "title parsed correctly" <|
        \url size title ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            , ( "title", Encode.string title )
            ]
                |> Encode.object
                |> Json.Decode.decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal (Ok title)



-- slidHueSetsHue : Test
-- slidHueSetsHue =
--   fuzz Fuzz.int "GotSlidHue sets the hue" <|
--     \amount ->
--       initialModel
--         |> update (GotSlidHue amount)
--         |> Tuple.first
--         |> .hue
--         |> Expect.equal amount


sliders : Test
sliders =
    let
        testSliders : String -> (Int -> Msg) -> (Model -> Int) -> Test
        testSliders description toMsg amountFromModel =
            fuzz Fuzz.int description <|
                \amount ->
                    initialModel
                        |> update (toMsg amount)
                        |> Tuple.first
                        |> amountFromModel
                        |> Expect.equal amount
    in
    describe "Slider sets the desired field in the Model"
        [ testSliders "GotSlidHue" GotSlidHue .hue
        , testSliders "GotSlidRipple" GotSlidRipple .ripple
        , testSliders "GotSlidNoise" GotSlidNoise .noise
        ]



-- Note: This test is brittle if we add images, e.g. a logo
--       it could be better to use Selector.containing and Selector.class


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos" <|
        \_ ->
            initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ Selector.tag "img" ]
                |> Query.count (Expect.equal 0)


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    let
        prefix =
            "http://elm-in-action.com/"
    in
    query
        |> Query.findAll
            [ Selector.tag "img"
            , Selector.attribute (Attr.src (prefix ++ url))
            ]
        |> Query.count (Expect.equal 1)


makeImageNamesFromCount : Int -> List String
makeImageNamesFromCount count =
    List.range 1 count
        |> List.map (\num -> String.fromInt num ++ ".png")


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }


thumbnailsWork : Test
thumbnailsWork =
    fuzz (urlFuzzer 1 5) "Urls rendered as thumbnails" <|
        \urls ->
            let
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks


getAt : Int -> List String -> String
getAt n xs =
    if n < 0 then
        ""

    else
        case List.head <| List.drop n xs of
            Nothing ->
                ""

            Just x ->
                x


thumbnailsLoadedFirstSelected : Test
thumbnailsLoadedFirstSelected =
    fuzz (urlFuzzer 1 5) "GotPhotos should return correct selected url" <|
        \urls ->
            let
                firstUrl =
                    getAt 0 urls

                photos =
                    List.map photoFromUrl urls
            in
            initialModel
                |> update (GotPhotos (Result.Ok photos))
                |> Tuple.first
                |> .status
                |> Expect.equal (Loaded photos firstUrl)


urlFuzzer : Int -> Int -> Fuzzer (List String)
urlFuzzer start stop =
    Fuzz.intRange start stop
        |> Fuzz.map makeImageNamesFromCount


thumbnailsClickPhotoWorks : Test
thumbnailsClickPhotoWorks =
    fuzz (urlFuzzer 2 5) "ClickPhoto should return correct selected url" <|
        \urls ->
            let
                secondUrl =
                    getAt 1 urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> update (ClickPhoto secondUrl)
                |> Tuple.first
                |> .status
                |> Expect.equal (Loaded (List.map photoFromUrl urls) secondUrl)


clickThumbnail : Test
clickThumbnail =
    fuzz3 (urlFuzzer 1 1) Fuzz.string (urlFuzzer 1 1) "Clicking a thumbnail selects it" <|
        \urlsBefore urlToSelect urlsAfter ->
            let
                url =
                    urlToSelect ++ ".png"

                photos =
                    (urlsBefore ++ url :: urlsAfter)
                        |> List.map photoFromUrl

                srcToClick =
                    prefix ++ url

                prefix =
                    "http://elm-in-action.com/"
            in
            { initialModel | status = Loaded photos "" }
                |> view
                |> Query.fromHtml
                |> Query.find [ Selector.tag "img", Selector.attribute (Attr.src srcToClick) ]
                |> Event.simulate Event.click
                |> Event.expect (ClickPhoto url)
