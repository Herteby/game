module World exposing (..)
import Simplex exposing (PermutationTable)
import Array exposing (Array)
import Game.TwoD.Render as Render
import Color

table : PermutationTable
table = Simplex.permutationTableFromInt 1



world : List (List Float)
world =
    List.range 0 100 |> List.map (\x ->
    List.range 0 100 |> List.map (\y ->
    Simplex.noise2d table (toFloat x / 10) (toFloat y / 10)
    )
    )



size = 1

render =
    world |>
    List.indexedMap (\x ->
    List.indexedMap (\y val ->
        Render.shape Render.rectangle
        {color = Color.rgb val val val
        ,position = (toFloat x*size,toFloat y*size)
        ,size = (size,size)

        }
    )
    ) |> List.concat
