module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error(..))

type alias Model =
  { board : List (List (Maybe Mark)) }

init : Int -> (Model, Cmd Msg)
init flags =
    ( { board =
        [ [Nothing, Nothing, Nothing]
        , [Nothing, Nothing, Nothing]
        , [Nothing, Nothing, Nothing]
        ]
      }
    , Cmd.none
    )

type Mark = X | O

-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg = NoOp | SetMark (Int, Int) Mark

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
      NoOp ->
        (model, Cmd.none)
      SetMark indexes mark ->
        case indexes of
          (0, 0) ->
            ( { model | board =
                [ [Just mark, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                ]
              }
            , Cmd.none
            )
          (0, 1) ->
            ( { model | board =
                [ [Nothing, Just mark, Nothing]
                , [Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                ]
              }
            , Cmd.none
            )
          (0, 2) ->
            ( { model | board =
                [ [Nothing, Nothing, Just mark]
                , [Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                ]
              }
            , Cmd.none
            )
          (1, 0) ->
            ( { model | board =
                [ [Nothing, Nothing, Nothing]
                , [Just mark, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                ]
              }
            , Cmd.none
            )
          (1, 1) ->
            ( { model | board =
                [ [Nothing, Nothing, Nothing]
                , [Nothing, Just mark, Nothing]
                , [Nothing, Nothing, Nothing]
                ]
              }
            , Cmd.none
            )
          (1, 2) ->
            ( { model | board =
                [ [Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Just mark]
                , [Nothing, Nothing, Nothing]
                ]
              }
            , Cmd.none
            )
          (2, 0) ->
            ( { model | board =
                [ [Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                , [Just mark, Nothing, Nothing]
                ]
              }
            , Cmd.none
            )
          (2, 1) ->
            ( { model | board =
                [ [Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                , [Nothing, Just mark, Nothing]
                ]
              }
            , Cmd.none
            )
          (2, 2) ->
            ( { model | board =
                [ [Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Just mark]
                ]
              }
            , Cmd.none
            )
          _ ->
            (model, Cmd.none)


-- ---------------------------
-- VIEW
-- ---------------------------

drawCell : Int -> Maybe Mark -> Html Msg
drawCell index mMark =
  div [ class ("cell-" ++ String.fromInt index)
      , onClick (SetMark (0, index) X)
      ]
      [ case mMark of
          Nothing -> text ""
          Just X -> text "X"
          Just O -> text "O"
      ]

drawRow : Int -> List (Maybe Mark) -> Html Msg
drawRow index row =
  div [ class ("row-" ++ String.fromInt index)] (List.indexedMap drawCell row)


view : Model -> Html Msg
view model =
  div []
    [ div
      [ style "margin" "0 auto"
      , style "width" "300px" ]
      (List.indexedMap drawRow model.board)
    ]




-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
