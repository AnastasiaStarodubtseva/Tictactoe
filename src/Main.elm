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
import Platform.Cmd exposing (none)

type alias Board = List (List (Maybe Mark))

type GameState
  = InProgress
  | Won Mark Position
  | Draw

type Position
  = NWSE
  | SWNE
  | MiddleColumn
  | MiddleRow
  | TopRow
  | BottomRow
  | LeftColumn
  | RightColumn

type alias Model =
  { board : Board
  , currentTurn : Mark
  }

boardIsFull : Board -> Bool
boardIsFull board =
  List.all (\c -> c /= Nothing) (List.concat board)

boardIsEmpty : Board -> Bool
boardIsEmpty board =
  List.all (\c -> c == Nothing) (List.concat board)

emptyBoard : Board
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

cellAt : Int -> Int -> Board -> Maybe Mark
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



determineGameState : Board -> GameState
determineGameState board =
  case board of
    [[Just X, Just X, Just X], _, _] -> Won X TopRow
    [[Just O, Just O, Just O], _, _] -> Won O TopRow
    [ _, [Just X, Just X, Just X], _] -> Won X MiddleRow
    [ _, [Just O, Just O, Just O], _] -> Won O MiddleRow
    [ _, _, [Just X, Just X, Just X]] -> Won X BottomRow
    [ _, _, [Just O, Just O, Just O]] -> Won O BottomRow
    [[ Just X, _, _ ], [ _, Just X, _ ], [ _, _, Just X ]] -> Won X NWSE
    [[ Just O, _, _ ], [ _, Just O, _ ], [ _, _, Just O]] -> Won O NWSE
    [[ _, _, Just X ], [ _, Just X, _ ], [ Just X, _, _ ]] -> Won X SWNE
    [[ _, _, Just O ], [ _, Just O, _ ], [ Just O, _, _ ]] -> Won O SWNE
    [[ _, Just X, _ ], [ _, Just X, _ ], [ _, Just X, _ ]] -> Won X MiddleColumn
    [[ _, Just O, _ ], [ _, Just O, _ ], [ _, Just O, _ ]] -> Won O MiddleColumn
    [[ Just X, _, _ ], [ Just X, _, _ ], [ Just X, _, _ ]] -> Won X LeftColumn
    [[ Just O, _, _ ], [ Just O, _, _ ], [ Just O, _, _ ]] -> Won O LeftColumn
    [[ _, _, Just X ], [ _, _, Just X ], [ _, _, Just X ]] -> Won X RightColumn
    [[ _, _, Just O ], [ _, _, Just O ], [ _, _, Just O ]] -> Won X RightColumn
    [[ Just _, Just _, Just _ ], [ Just _, Just _, Just _ ], [ Just _, Just _, Just _ ]] -> Draw
    _ -> InProgress


showMark : Mark -> String
showMark mark =
  case mark of
    X -> "X"
    O -> "O"
-- ---------------------------
-- VIEW
-- ---------------------------
drawCell : Model -> Int -> Int -> Maybe Mark -> Html Msg
drawCell model rowIndex index mMark =
  div [ if mMark == Nothing
        then style "cursor" "pointer"
        else style "cursor" "default"
      , class ("cell-" ++ String.fromInt index)
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
  div [ class ("innerContainer-" ++ String.fromInt index)] (List.indexedMap (drawCell model index) row)

positionMarker : GameState -> Html a
positionMarker state =
  div
    [ class "position-marker"
    , class <| case state of
        Won _ TopRow -> "top-row"
        Won _ MiddleRow -> "middle-row"
        Won _ BottomRow -> "bottom-row"
        Won _ LeftColumn -> "left-column"
        Won _ MiddleColumn -> "middle-column"
        Won _ RightColumn -> "right-column"
        Won _ NWSE -> "nwse"
        Won _ SWNE -> "swne"
        _ -> "hidden"
    ] []

view : Model -> Html Msg
view model =
  div [ ]
    [ div
      [ class "container"
      , style "margin" "0 auto"
      , style "width" "300px"
      , style "position" "relative"]
      (positionMarker (determineGameState model.board) :: (List.indexedMap (drawRow model) model.board))
    , div [class "game-state"]
      [ if boardIsEmpty model.board
        then text ""
        else
          case determineGameState model.board of
            InProgress -> text "Game in progress"
            Won mark position -> text ("The winner is " ++ showMark mark)
            Draw ->  text "No winner"
      ]
    , button [class "reset-button", onClick Reset] [text "Restart the game"]
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
