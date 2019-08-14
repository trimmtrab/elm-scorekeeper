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

    Save ->
      if (String.isEmpty model.name) then
        Debug.log "Save Updated Model"
          model
      else
        Debug.log "Save Updated Model"
          (save model)

    -- _ acts like a wildcard
    _ ->
      model

save : Model -> Model
save model =
  case model.playerId of
    Just id ->
      -- edit model id
      model

    Nothing ->
      addPlayer model

addPlayer : Model -> Model
addPlayer model =
  let
    player =
      Player (List.length model.players) model.name 0

    newPlayers =
      -- :: adds an element to a list beginning
      -- (less expensive than ++)
      player :: model.players
  in
    { model
    | players = newPlayers
    , name = ""
    }

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
