module GamePage exposing (..)

import AltMath.Vector2 as Vec2
import Character
import Dict exposing (Dict)
import Html exposing (Html)
import Minimap
import Playground exposing (Computer, Game, Shape)
import Playground.Advanced as Playground
import Terrain
import Time
import Types exposing (..)
import World


init : Account -> Dict String Character -> ( Playground.Game Memory, Cmd Playground.Msg )
init account others =
    game.init
        |> Tuple.mapFirst
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
                    }
                )
            )


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
        }


render : Computer -> Memory -> List Shape
render computer { player, others, chunks } =
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
                        case chunk of
                            Pending ->
                                Nothing

                            Received chunk_ ->
                                if abs diff.x < 1 && abs diff.y < 1 then
                                    World.render chunk_
                                        |> Playground.move
                                            (toFloat x * World.chunkSize * Terrain.tileSize)
                                            (toFloat y * World.chunkSize * Terrain.tileSize)
                                        |> Just

                                else
                                    Nothing
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
           ]


updateGame : Computer -> Memory -> Memory
updateGame computer memory =
    { memory
        | player = Character.update computer memory
        , others = Dict.map (\_ ( c, coords ) -> ( c, Character.interpolate computer.time c coords )) memory.others
        , chunks =
            Dict.filter
                (\( x, y ) _ ->
                    let
                        chunkVec =
                            { x = toFloat x - 0.5, y = toFloat y - 0.5 }

                        diff =
                            Vec2.sub chunkVec (Vec2.scale (1 / World.chunkSize / Terrain.tileSize) memory.player.coords)
                    in
                    abs diff.x < 5 && abs diff.y < 5
                )
                memory.chunks
        , fps = 1000 // Playground.delta computer.time :: memory.fps |> List.take 60
    }
