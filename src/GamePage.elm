module GamePage exposing (..)

import AltMath.Vector2 as Vec2
import Character exposing (Character, Direction(..))
import Dict exposing (Dict)
import Playground exposing (Computer, Shape)
import Playground.Advanced as Playground
import Terrain
import Types exposing (..)
import World


init : Account -> Dict String Character -> ( Playground.Game Memory, Cmd Playground.Msg )
init account others =
    game.init
        |> Tuple.mapFirst
            (Playground.edit
                (\_ _ ->
                    { player = account.character
                    , others = others
                    , chunks = Dict.empty
                    , messages = []
                    , chatInput = Nothing
                    , messageI = 0
                    , showPlayerList = False
                    }
                )
            )


game =
    Playground.embed render
        updateGame
        { player =
            { coords = { x = 0, y = 0 }
            , direction = Down
            , moving = False
            , skin = 1
            }
        , others = Dict.empty
        , chunks = Dict.empty
        , messages = []
        , chatInput = Nothing
        , messageI = 0
        , showPlayerList = False
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
                                { x = toFloat x - 0.5, y = toFloat y - 0.5 }

                            diff =
                                Vec2.sub chunkVec (Vec2.scale (1 / World.chunkSize / Terrain.tileSize) player.coords)
                        in
                        case chunk of
                            Pending ->
                                Nothing

                            Received chunk_ ->
                                if diff.x < 1 && diff.y < 1 then
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
                ++ ((player :: Dict.values others)
                        |> List.sortBy (.coords >> .y >> negate)
                        |> List.map (Character.render computer.time)
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
    { memory | player = Character.update computer memory }
