module Main exposing (..)

import Browser exposing (sandbox)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- model

type alias Model =
  { players : List Player
  , name : String
  , playerId : Maybe Int
  , plays : List Play
  }

type alias Player =
  { id : Int
  , name : String
  , points : Int
  }

type alias Play =
  { id : Int
  , playerId : Int
  , name : String
  , points : Int
  }

initModel : Model
initModel =
  { players = []
  , name = ""
  , playerId = Nothing
  , plays = []
  }

-- update

type Msg
  = Edit Player
  | Score Player Int
  | Input String
  | Save
  | Cancel
  | DeletePlay Play

update: Msg -> Model -> Model
update msg model =
  case msg of
    Cancel ->
      Debug.log "Cancel Updated Model"
        { model | name = "", playerId = Nothing }

    Input name ->
      Debug.log "Input Updated Model"
        { model | name = name }

    -- _ acts like a wildcard
    _ ->
      model

-- view

view : Model -> Html Msg
view model =
  div [ class "scoreboard" ]
      [ h1 [] [ text "Score Keeper" ]
      , playerForm model
      ]

playerForm : Model -> Html Msg
playerForm model =
  Html.form [ onSubmit Save ]
    [ input
        [ type_ "text"
        , placeholder "Add/Edit Player..."
        , onInput Input
        , value model.name
        ]
        []
    , button [ type_ "submit" ] [ text "Save" ]
    , button [ type_ "button", onClick Cancel ] [ text "Cancel" ]
    ]

main : Program () Model Msg
main =
  Browser.sandbox
    { init = initModel
    , view = view
    , update = update
    }
