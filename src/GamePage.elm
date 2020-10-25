module GamePage exposing (..)

import Account exposing (Account)
import AltMath.Vector2 as Vec2 exposing (Vec2)
import Browser.Dom
import Browser.Events
import Character exposing (Character, Direction(..), Speed(..))
import Chunk exposing (Chunk)
import Dict exposing (Dict)
import FontAwesome.Icon exposing (Icon)
import FontAwesome.Solid as Solid
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Html.Lazy exposing (..)
import Json.Decode as Decode
import Lamdera
import Minimap
import Playground exposing (Computer, Game, Shape)
import Playground.Advanced as Playground
import Task
import Terrain
import Time
import Types exposing (..)
import UI exposing (..)
import UI.Icon as Icon
import World


class =
    namespace "GamePage"


init : Account -> Dict String Character -> ( Game Memory, Cmd GameMsg )
init account others =
    game.init
        |> Tuple.mapBoth
            (Playground.edit
                (\_ _ ->
                    { player = account.character
                    , others = Dict.map (\_ c -> ( c, c.coords )) others
                    , chunks = Dict.empty
                    , messages = []
                    , chatInput = Nothing
                    , messageI = 0
                    , showPlayerList = False
                    , lastUpdate = ( Time.millisToPosix 0, account.character )
                    , fps = []
                    , showMinimap = False
                    }
                )
            )
            (Cmd.map PlaygroundMsg)


subscriptions : Game Memory -> Sub GameMsg
subscriptions model =
    Sub.batch
        [ Sub.map PlaygroundMsg Playground.subscriptions.all
        , Browser.Events.onKeyDown (Decode.field "code" Decode.string |> Decode.map KeyDown)
        ]


update : GameMsg -> Game Memory -> ( Game Memory, Cmd GameMsg )
update msg model =
    case msg of
        PlaygroundMsg submsg ->
            game.update submsg model
                |> (\( newmodel, cmd ) ->
                        let
                            ( computer, newMemory ) =
                                Playground.get newmodel

                            ( cx, cy ) =
                                newMemory.player.coords
                                    |> (\{ x, y } ->
                                            ( round <| x / (64 * 16) - 0.5, round <| y / (64 * 16) - 0.5 )
                                       )

                            ( lastUpdate, cmd_ ) =
                                if newMemory.player /= Tuple.second newMemory.lastUpdate && (Playground.now computer.time > Time.posixToMillis (Tuple.first newMemory.lastUpdate) + 250) then
                                    ( ( Time.millisToPosix <| Playground.now computer.time, newMemory.player )
                                    , Lamdera.sendToBackend (UpdatePlayer newMemory.player)
                                    )

                                else
                                    ( newMemory.lastUpdate, Cmd.none )

                            newNewModel =
                                newmodel |> Playground.edit (\_ mem -> { mem | lastUpdate = lastUpdate })
                        in
                        ( newNewModel
                        , Cmd.batch
                            [ Cmd.map PlaygroundMsg cmd
                            , cmd_
                            ]
                        )
                            |> checkChunk cx cy
                            |> checkChunk (cx + 1) cy
                            |> checkChunk cx (cy + 1)
                            |> checkChunk (cx + 1) (cy + 1)
                   )

        KeyDown code ->
            if code == "Enter" then
                let
                    ( _, memory ) =
                        Playground.get model
                in
                case memory.chatInput of
                    Just message ->
                        ( model |> Playground.edit (\_ mem -> { mem | chatInput = Nothing })
                        , if message == "" then
                            Cmd.none

                          else
                            Lamdera.sendToBackend (SendMessage message)
                        )

                    Nothing ->
                        ( model |> Playground.edit (\_ mem -> { mem | chatInput = Just "" })
                        , Browser.Dom.focus "chatInput" |> Task.attempt (always Noop2)
                        )

            else
                ( model, Cmd.none )

        ChatInput message ->
            ( model |> Playground.edit (\_ mem -> { mem | chatInput = Just message }), Cmd.none )

        ToggleMinimap ->
            ( model |> Playground.edit (\_ mem -> { mem | showMinimap = not mem.showMinimap }), Cmd.none )

        TogglePlayerList ->
            ( model |> Playground.edit (\_ mem -> { mem | showPlayerList = not mem.showPlayerList }), Cmd.none )

        RemoveMessage i ->
            ( model
                |> Playground.edit (\_ mem -> { mem | messages = List.filter (\( i_, m ) -> i_ /= i) mem.messages })
            , Cmd.none
            )

        Noop2 ->
            ( model, Cmd.none )


checkChunk x y ( model, cmd ) =
    let
        memory =
            Playground.get model |> Tuple.second
    in
    if Dict.get ( x, y ) memory.chunks == Nothing then
        ( model |> Playground.edit (\_ _ -> { memory | chunks = Dict.insert ( x, y ) Nothing memory.chunks })
        , Cmd.batch [ cmd, Lamdera.sendToBackend (GetChunk x y) ]
        )

    else
        ( model, cmd )


view : Game Memory -> Html GameMsg
view gamemodel =
    let
        ( computer, memory ) =
            Playground.get gamemodel
    in
    div []
        [ game.view gamemodel
        , lazy2 chat memory.messages memory.chatInput
        , info memory
        , namePlates memory.player memory.others
        , if memory.showPlayerList then
            playerList memory.others

          else
            none
        ]


info : Memory -> Html msg
info { fps, player } =
    div [ class "coords" ]
        [ div [] [ text <| "fps: " ++ (String.fromInt <| List.sum fps // List.length fps) ]
        , div [] [ text ("x: " ++ String.fromInt (round player.coords.x)) ]
        , div [] [ text ("y: " ++ String.fromInt (round player.coords.y)) ]
        ]


chat : List ( Int, Message ) -> Maybe String -> Html GameMsg
chat messages chatInput =
    div [ class "bottomLeft" ]
        [ div [ class "chat" ]
            [ messages
                |> List.reverse
                |> List.map
                    (\( i, m ) ->
                        ( String.fromInt i
                        , case m of
                            UserMessage { username, skin, message } ->
                                div [ class "message" ]
                                    [ div [ class "avatar", style "background-image" ("url(" ++ Character.url skin ++ ")") ] []
                                    , div []
                                        [ div [ class "username" ] [ text username ]
                                        , div [] [ text message ]
                                        ]
                                    ]

                            SystemMessage message ->
                                div [ class "system message" ]
                                    [ text message
                                    ]
                        )
                    )
                |> Html.Keyed.node "div" []
            , case chatInput of
                Just message ->
                    input
                        [ class "chatInput"
                        , value message
                        , onInput ChatInput
                        , id "chatInput"
                        ]
                        []

                Nothing ->
                    div [ class "chatHint" ] [ text "Press enter to chat" ]
            ]
        , uiButton Solid.users "Open player list" TogglePlayerList
        , uiButton Solid.map "Show map" ToggleMinimap
        ]


uiButton : Icon -> String -> msg -> Html msg
uiButton icon title msg =
    button
        [ class "uiButton"
        , onClick msg
        , Attributes.title title
        ]
        [ Icon.view [] icon ]


namePlates : Character -> Dict String ( Character, Vec2 ) -> Html GameMsg
namePlates player others =
    div [ class "namePlates overlay" ]
        [ Dict.toList others
            |> List.map
                (\( username, ( _, { x, y } ) ) ->
                    div
                        [ class "namePlate"
                        , style "top" (px ((player.coords.y - y) * 2))
                        , style "left" (px ((x - player.coords.x) * 2))
                        ]
                        [ text username ]
                )
            |> div []
        ]


playerList : Dict String ( Character, Vec2 ) -> Html GameMsg
playerList players =
    div [ class "overlay", onClick TogglePlayerList ]
        [ div [ class "modal" ]
            [ div [ class "header" ] [ textSpan "Players" ]
            , div [ class "body" ]
                (Dict.toList players
                    |> List.map
                        (\( username, ( character, coords ) ) ->
                            div [ class "player" ]
                                [ div [ class "avatar", style "background-image" ("url(" ++ Character.url character.skin ++ ")") ] []
                                , textSpan username
                                , textSpan ("x: " ++ String.fromInt (round character.coords.x))
                                , textSpan ("y: " ++ String.fromInt (round character.coords.y))
                                ]
                        )
                )
            ]
        ]


game :
    { init : ( Game Memory, Cmd Playground.Msg )
    , view : Game Memory -> Html a
    , update : Playground.Msg -> Game Memory -> ( Game Memory, Cmd Playground.Msg )
    }
game =
    let
        char =
            { coords = { x = 0, y = 0 }
            , direction = Down
            , speed = Standing
            , skin = 1
            }
    in
    Playground.embed render
        updateGame
        { player = char
        , others = Dict.empty
        , chunks = Dict.empty
        , messages = []
        , chatInput = Nothing
        , messageI = 0
        , showPlayerList = False
        , lastUpdate = ( Time.millisToPosix 0, char )
        , fps = []
        , showMinimap = False
        }


render : Computer -> Memory -> List Shape
render computer { player, others, chunks, showMinimap } =
    let
        terrain =
            chunks
                |> Dict.toList
                |> List.sortBy (Tuple.first >> Tuple.second)
                |> List.reverse
                |> List.filterMap
                    (\( ( x, y ), chunk ) ->
                        let
                            chunkVec =
                                { x = toFloat x, y = toFloat y }

                            diff =
                                Vec2.sub chunkVec (Vec2.scale (1 / World.chunkSize / Terrain.tileSize) player.coords)
                        in
                        Maybe.andThen
                            (\chunk_ ->
                                if abs diff.x < 1 && abs diff.y < 1 then
                                    World.render chunk_
                                        |> Playground.move
                                            (toFloat x * World.chunkSize * Terrain.tileSize)
                                            (toFloat y * World.chunkSize * Terrain.tileSize)
                                        |> Just

                                else
                                    Nothing
                            )
                            chunk
                    )
    in
    Playground.square Playground.black 2000
        :: [ terrain
                ++ (((player |> (\c -> ( c, c.coords ))) :: Dict.values others)
                        |> List.sortBy (Tuple.second >> .y >> negate)
                        |> List.map (\( c, coords ) -> Character.render computer.time c |> Playground.move coords.x coords.y)
                   )
                |> Playground.group
                |> (if computer.keyboard.space then
                        Playground.move (negate player.coords.x) (negate player.coords.y)

                    else
                        Playground.scale 2
                            >> Playground.move (negate player.coords.x * 2) (negate player.coords.y * 2)
                   )
           , if showMinimap then
                Minimap.render chunks |> Playground.fade 0.8

             else
                Playground.square Playground.black 0
           ]


updateGame : Computer -> Memory -> Memory
updateGame computer memory =
    { memory
        | player =
            if memory.chatInput == Nothing then
                Character.update computer memory.player

            else
                memory.player
        , others = Dict.map (\_ ( c, coords ) -> ( c, Character.interpolate computer.time c coords )) memory.others
        , fps = 1000 // Playground.delta computer.time :: memory.fps |> List.take 60
    }
