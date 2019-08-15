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
      { model | name = "", playerId = Nothing }

    DeletePlay play ->
      deletePlay model play

    Edit player ->
      { model
        | name = player.name
        , playerId = Just player.id
        }

    Input name ->
      { model | name = name }

    Save ->
      if (String.isEmpty model.name) then
        model
      else
        (save model)

    Score player points ->
      (score model player points)

deletePlay : Model -> Play -> Model
deletePlay model play =
  let
    newPlays =
      List.filter (\p -> p.id /= play.id) model.plays

    newPlayers =
      List.map
        (\ player ->
          if player.id == play.playerId then
            { player | points = player.points - play.points }
          else
            player
        )
        model.players
  in
    { model
    | players = newPlayers
    , plays = newPlays
    }

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
      , playSection model
      ]

playSection : Model -> Html Msg
playSection model =
  div []
      [ playListHeader
      , playList model
      ]

playListHeader  : Html msg
playListHeader =
  header []
    [ div [] [ text "Plays" ]
    , div [] [ text "Points" ]
    ]

playList : Model -> Html Msg
playList model =
  model.plays
    |> List.map playItem
    |> ul []

playItem : Play -> Html Msg
playItem play =
  li []
    [ i
      [ class "remove"
      , onClick (DeletePlay play)
      ]
      []
    , div [] [ text play.playerName ]
    , div [] [ text (String.fromInt play.points) ]
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
    |> List.map (playerItem model.playerId)
    |> ul []

playerItem : Maybe Int -> Player -> Html Msg
playerItem playerId player =
  let
    itemClass =
      case playerId of
        Just id ->
          if player.id == id then
            "editedPlayer"
          else
            ""
        Nothing ->
          ""
  in
    li []
      [ i
        [ class "edit"
        , onClick (Edit player)
        ]
        []
      , div [ class itemClass ]
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
  let
    inputClass =
      case model.playerId of
        Just id ->
          "editedPlayer"
        Nothing ->
          ""
  in
    Html.form [ onSubmit Save ]
      [ input
          [ class inputClass
          , type_ "text"
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
