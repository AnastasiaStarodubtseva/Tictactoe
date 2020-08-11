module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode exposing (index)
import List.Extra exposing (getAt)
import Html exposing (button)
import Array exposing (empty)

type alias Model =
  { board : List (List (Maybe Mark))
  , currentTurn : Mark
  }

boardIsFull : List (List (Maybe Mark)) -> Bool
boardIsFull board =
  List.all (\c -> c /= Nothing) (List.concat board)

emptyBoard : List (List (Maybe Mark))
emptyBoard =
  [ [Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Nothing]
  ]

init : Int -> (Model, Cmd Msg)
init flags =
    ( { board = emptyBoard
      , currentTurn = X
      }
    , Cmd.none
    )

type Mark = X | O

-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg = NoOp | SetMark (Int, Int) Mark | Reset

cellAt : Int -> Int -> List (List (Maybe Mark)) -> Maybe Mark
cellAt row column board =
  board
    |> getAt row
    |> Maybe.andThen (getAt column)
    |> Maybe.withDefault Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
      NoOp ->
        (model, Cmd.none)
      Reset ->
        ({ model | board = emptyBoard }, Cmd.none)
      SetMark indexes mark ->
        case indexes of
          (0, 0) ->
            ( { model | board =
                [ [Just mark, cellAt 0 1 model.board, cellAt 0 2 model.board]
                , [cellAt 1 0  model.board, cellAt 1 1 model.board, cellAt 1 2 model.board]
                , [cellAt 2 0 model.board, cellAt 2 1 model.board, cellAt 2 2 model.board]
                ]
              , currentTurn =
                  case model.currentTurn of
                    X -> O
                    O -> X
              }
            , Cmd.none
            )
          (0, 1) ->
            ( { model | board =
                [ [cellAt 0 0 model.board, Just mark, cellAt 0 2 model.board]
                , [cellAt 1 0 model.board, cellAt 1 1 model.board, cellAt 1 2 model.board]
                , [cellAt 2 0 model.board, cellAt 2 1 model.board, cellAt 2 2 model.board]
                ]
                , currentTurn =
                  case model.currentTurn of
                    X -> O
                    O -> X
              }
            , Cmd.none
            )
          (0, 2) ->
            ( { model | board =
                [ [cellAt 0 0 model.board, cellAt 0 1 model.board, Just mark]
                , [cellAt 1 0 model.board, cellAt 1 0 model.board, cellAt 1 2 model.board]
                , [cellAt 2 0 model.board, cellAt 2 1 model.board, cellAt 2 2 model.board]
                ]
                , currentTurn =
                  case model.currentTurn of
                    X -> O
                    O -> X
              }
            , Cmd.none
            )
          (1, 0) ->
            ( { model | board =
                [ [cellAt 0 0 model.board, cellAt 0 1 model.board, cellAt 0 2 model.board]
                , [Just mark, cellAt 1 1 model.board, cellAt 1 2 model.board]
                , [cellAt 2 0 model.board, cellAt 2 1 model.board, cellAt 2 2 model.board]
                ]
                , currentTurn =
                  case model.currentTurn of
                    X -> O
                    O -> X
              }
            , Cmd.none
            )
          (1, 1) ->
            ( { model | board =
                [ [cellAt 0 0 model.board, cellAt 0 1 model.board, cellAt 0 2 model.board]
                , [cellAt 1 0 model.board, Just mark, cellAt 1 2 model.board]
                , [cellAt 2 0 model.board, cellAt 2 1 model.board, cellAt 2 2 model.board]
                ]
                , currentTurn =
                  case model.currentTurn of
                    X -> O
                    O -> X
              }
            , Cmd.none
            )
          (1, 2) ->
            ( { model | board =
                [ [cellAt 0 0 model.board, cellAt 0 1 model.board, cellAt 0 2 model.board]
                , [cellAt 1 0 model.board, cellAt 1 1 model.board, Just mark]
                , [cellAt 2 0 model.board, cellAt 2 1 model.board, cellAt 2 2 model.board]
                ]
                , currentTurn =
                  case model.currentTurn of
                    X -> O
                    O -> X
              }
            , Cmd.none
            )
          (2, 0) ->
            ( { model | board =
                [ [cellAt 0 0 model.board, cellAt 0 1 model.board, cellAt 0 2 model.board]
                , [cellAt 1 0 model.board, cellAt 1 1 model.board, cellAt 1 2 model.board]
                , [Just mark, cellAt 2 1 model.board, cellAt 2 2 model.board]
                ]
                , currentTurn =
                  case model.currentTurn of
                    X -> O
                    O -> X
              }
            , Cmd.none
            )
          (2, 1) ->
            ( { model | board =
                [ [cellAt 0 0 model.board, cellAt 0 1 model.board, cellAt 0 2 model.board]
                , [cellAt 1 0 model.board, cellAt 1 1 model.board, cellAt 1 2 model.board]
                , [cellAt 2 0 model.board, Just mark, cellAt 2 2 model.board]
                ]
                , currentTurn =
                  case model.currentTurn of
                    X -> O
                    O -> X
              }
            , Cmd.none
            )
          (2, 2) ->
            ( { model | board =
                [ [cellAt 0 0 model.board, cellAt 0 1 model.board, cellAt 0 2 model.board]
                , [cellAt 1 0 model.board, cellAt 1 1 model.board, cellAt 1 2 model.board]
                , [cellAt 2 0 model.board, cellAt 2 1 model.board, Just mark]
                ]
                , currentTurn =
                  case model.currentTurn of
                    X -> O
                    O -> X
              }
            , Cmd.none
            )
          _ ->
            (model, Cmd.none)


-- ---------------------------
-- VIEW
-- ---------------------------

drawCell : Model -> Int -> Int -> Maybe Mark -> Html Msg
drawCell model rowIndex index mMark =
  div [ class ("cell-" ++ String.fromInt index)
      , onClick <| case mMark of
          Nothing -> (SetMark (rowIndex, index) model.currentTurn)
          Just _ -> NoOp
      ]
      [ case mMark of
          Nothing -> text ""
          Just X -> text "X"
          Just O -> text "O"
      ]

drawRow : Model -> Int -> List (Maybe Mark) -> Html Msg
drawRow model index row =
  div [ class ("row-" ++ String.fromInt index)] (List.indexedMap (drawCell model index) row)


view : Model -> Html Msg
view model =
  div []
    [ div
      [ style "margin" "0 auto"
      , style "width" "300px" ]
      (List.indexedMap (drawRow model) model.board)
    , if boardIsFull model.board
      then div []  [text "Game is finished"]
      else text "Game is not finished"
    , button [onClick Reset] [text "Restart"]
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
