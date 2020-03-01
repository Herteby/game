module World exposing (..)
import Simplex exposing (PermutationTable)
import Array exposing (Array)
import Color
import Playground
import Playground.Extra as Playground
import Image

table : PermutationTable
table = Simplex.permutationTableFromInt 1



world : List (List Float)
world =
    List.range 0 30 |> List.map (\x ->
    List.range 0 30 |> List.map (\y ->
    Simplex.noise2d table (toFloat x / 10) (toFloat y / 10)
    )
    )

image = Image.fromList2d (List.map (List.map (\f -> ((f + 1) * 4) +1  |> round) )world)

imageUrl = Image.toPngUrl image

size = 32

render =
    [Playground.tilemap 16 16 "/ground.png" imageUrl |> Playground.scaleX 3 |> Playground.scaleY 3]