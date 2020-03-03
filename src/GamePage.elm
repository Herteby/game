module GamePage exposing (..)

import Character exposing (Character, Direction(..))
import Dict
import Playground exposing (Computer, Shape)
import Playground.Advanced as Playground
import Types exposing (..)
import World


init : Account -> ( Playground.Game Memory, Cmd Playground.Msg )
init account =
    game.init
        |> Tuple.mapFirst (Playground.edit (\_ memory -> { memory | character = account.character }))


game =
    Playground.embed render
        updateGame
        { character =
            { coords = ( 0, 0 )
            , direction = Down
            , moving = False
            , variant = 1
            }
        , others = Dict.empty
        }


render : Computer -> Memory -> List Shape
render computer { character, others } =
    World.render
        ++ ((character :: Dict.values others)
                |> List.map (Character.render computer.time)
           )
        |> List.map
            (Playground.scale 3 >> Playground.move (negate (Tuple.first character.coords)) (negate (Tuple.second character.coords)))


updateGame : Computer -> Memory -> Memory
updateGame computer memory =
    { memory | character = Character.update computer memory.character }
