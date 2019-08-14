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
  , playerName : String
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

    Edit player ->
      Debug.log "Edit Updated Model"
        { model
        | name = player.name
        , playerId = Just player.id
        }

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

    Score player points ->
      Debug.log "Score Updated Model"
        (score model player points)

    -- _ acts like a wildcard
    _ ->
      model

score: Model -> Player -> Int -> Model
score model scorer points =
  let
    newPlayers =
      List.map
        (\ player ->
          if player.id == scorer.id then
            { player
            | points = player.points + points
            }
          else
            player
        )
        model.players
    play =
      Play (List.length model.plays) scorer.id scorer.name points
  in
    { model
    | players = newPlayers
    , plays = play :: model.plays
    }


save : Model -> Model
save model =
  case model.playerId of
    Just id ->
      editPlayer model id

    Nothing ->
      addPlayer model

editPlayer : Model -> Int -> Model
editPlayer model id =
  let
    newPlayers =
      List.map
        -- \ denotes an anonymous function
        (\ player ->
          if player.id == id then
            { player | name = model.name }
          else
            player
        )
        model.players

    newPlays =
      List.map
        (\ play ->
          if play.playerId == id then
            { play | playerName = model.name }
          else
            play
        )
        model.plays

  in
    { model
    | name = ""
    , playerId = Nothing
    , players = newPlayers
    , plays = newPlays
    }

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
      , playerSection model
      , playerForm model
      ]

playerSection : Model -> Html Msg
playerSection model =
  div []
      [ playerListHeader
      , playerList model
      , pointTotal model
      ]

playerListHeader : Html Msg
playerListHeader =
  header []
    [ div [] [ text "Name" ]
    , div [] [ text "Points"]
    ]

playerList : Model -> Html Msg
playerList model =
  model.players
    |> List.sortBy .name
    |> List.map playerItem
    |> ul []

playerItem : Player -> Html Msg
playerItem player =
  li []
    [ i
      [ class "edit"
      , onClick (Edit player)
      ]
      []
    , div []
      [ text player.name ]
    , button
      [ type_ "button"
      , onClick (Score player 2)
      ]
      [ text "2pt" ]
    , button
      [ type_ "button"
      , onClick (Score player 3)
      ]
      [ text "3pt" ]
    , div []
      [ text (String.fromInt player.points) ]
    ]

pointTotal : Model -> Html Msg
pointTotal model =
  let
    total =
      List.map .points model.plays
        |> List.sum
  in
    footer []
      [ div [] [ text "Total:" ]
      , div [] [ text (String.fromInt total) ]
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
