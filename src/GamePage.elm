module GamePage exposing (..)

import Character exposing (Character, Direction(..))
import Dict exposing (Dict)
import Playground exposing (Computer, Shape)
import Playground.Advanced as Playground
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
                    }
                )
            )


game =
    Playground.embed render
        updateGame
        { player =
            { coords = ( 0, 0 )
            , direction = Down
            , moving = False
            , skin = 1
            }
        , others = Dict.empty
        , chunks = Dict.empty
        , messages = []
        , chatInput = Nothing
        , messageI = 0
        }


render : Computer -> Memory -> List Shape
render computer { player, others, chunks } =
    let
        terrain =
            chunks
                |> Dict.toList
                |> List.concatMap
                    (\( ( x, y ), chunk ) ->
                        case chunk of
                            Pending ->
                                []

                            Received chunk_ ->
                                [ World.render chunk_
                                    |> Playground.move
                                        (toFloat x * 64 * 16)
                                        (toFloat y * 64 * 16)
                                ]
                    )
    in
    [ List.map (Playground.scale 0.5) terrain
        ++ ((player :: Dict.values others)
                |> List.sortBy (.coords >> Tuple.second >> negate)
                |> List.map (Character.render computer.time)
           )
        |> Playground.group
        |> (if computer.keyboard.space then
                Playground.move (negate (Tuple.first player.coords)) (negate (Tuple.second player.coords))

            else
                Playground.scale 3
                    >> Playground.move (negate (Tuple.first player.coords) * 3) (negate (Tuple.second player.coords) * 3)
           )
    ]


updateGame : Computer -> Memory -> Memory
updateGame computer memory =
    { memory | player = Character.update computer memory }
