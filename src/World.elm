module World exposing (..)

import Image
import Playground
import Playground.Extra as Playground
import Simplex exposing (PermutationTable)


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 1


world : List (List Float)
world =
    List.range 0 100
        |> List.map
            (\x ->
                List.range 0 100
                    |> List.map
                        (\y ->
                            fractal2d standard permTable (toFloat x / 10) (toFloat y / 10)
                        )
            )


image =
    Image.fromList2d (List.map (List.map (\f -> ((f + 1) * 4) + 1 |> round)) world)


imageUrl =
    Image.toPngUrl image


size =
    32


render =
    [ Playground.tilemap 16 16 "/ground.png" imageUrl
        |> Playground.scale 3
    ]


type alias FractalConfig =
    { steps : Int
    , persistence : Float
    }


standard : FractalConfig
standard =
    { steps = 2
    , persistence = 2
    }


fractal2d : FractalConfig -> PermutationTable -> Float -> Float -> Float
fractal2d { steps, persistence } table x y =
    List.range 0 (steps - 1)
        |> List.map toFloat
        |> List.foldl
            (\step ( noise, max ) ->
                let
                    freq =
                        2 ^ step

                    amp =
                        persistence ^ step
                in
                ( noise + (amp * Simplex.noise2d table (x / freq) (y / freq))
                , max + amp * 0.7
                )
            )
            ( 0, 0 )
        |> (\( noise, max ) -> noise / (2 ^ toFloat steps))
