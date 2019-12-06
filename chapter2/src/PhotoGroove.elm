module PhotoGroove exposing (main)

-- Elm Runtime (ER)
-- Msg -> update -> Model -> view -> Html
-- ER produces a Msg which triggers the `update`
-- function. `update` modifies the `Model`
-- `view` takes the Model and produces Html

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (class, id, src, classList)


initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


viewThumbnail selectedUrl thumb =
    img
        -- (src ("http://elm-in-action.com/" ++ thumb.url)
        --     :: (if selectedUrl == thumb.url then
        --             [ class "selected" ]

        --         else
        --             []
        --        )
        -- )
        -- is equivalent to:
        [ src ("http://elm-in-action.com/" ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        ]
        []


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Grove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
          [ class "large"
          , src ("http://elm-in-action.com/large/" ++ model.selectedUrl)
          ]
          []
        ]


main =
    view initialModel
