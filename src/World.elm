module World exposing (..)

import Image
import Matrix exposing (Matrix)
import Maybe.Extra as Maybe
import Playground
import Playground.Extra as Playground
import Simplex exposing (PermutationTable)


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 1


terrainMap =
    Playground.tilemap 32 32 "/grass.png"


worldFloat : Matrix Float
worldFloat =
    List.range 0 66
        |> List.map
            (\x ->
                List.range 0 66
                    |> List.map
                        (\y ->
                            fractal2d standard permTable (toFloat x) (toFloat y)
                        )
            )
        |> Matrix.fromLists
        |> Maybe.withDefault Matrix.empty
        |> Debug.log "world"


world : Matrix Bool
world =
    Matrix.map ((>) 0) worldFloat


image =
    List.range 1 64
        |> List.reverse
        |> List.map
            (\y ->
                List.range 1 65
                    |> List.map
                        (\x ->
                            case getNeighbors x y world of
                                Just n ->
                                    stuff n

                                Nothing ->
                                    0
                        )
            )
        |> Image.fromList2d
        |> Image.toPngUrl


stuff : Neighbors Bool -> Int
stuff { topLeft, top, topRight, left, center, right, bottomLeft, bottom, bottomRight } =
    if center then
        if topLeft && top && topRight && left && right && bottomLeft && bottom && bottomRight then
            11

        else if topLeft && top && topRight && left && right && bottomLeft && bottom then
            2

        else if topLeft && top && topRight && left && right && bottom && bottomRight then
            3

        else if topLeft && top && left && right && bottomLeft && bottom && bottomRight then
            5

        else if top && topRight && left && right && bottomLeft && bottom && bottomRight then
            6

        else if left && right && bottomLeft && bottom && bottomRight then
            8

        else if top && topRight && right && bottom && bottomRight then
            10

        else if topLeft && top && left && bottomLeft && bottom then
            12

        else if topLeft && top && topRight && left && right then
            14

        else if top && topRight && left && right && bottomLeft && bottom then
            19

        else if topLeft && top && left && right && bottom && bottomRight then
            20

        else if center && right && bottom && bottomRight then
            7

        else if left && bottomLeft && bottom then
            9

        else if top && topRight && right then
            13

        else if topLeft && top && left then
            15

        else
            1

    else
        0


render =
    [ terrainMap image
    ]


type alias FractalConfig =
    { steps : Int
    , persistence : Float
    , scale : Float
    }


standard : FractalConfig
standard =
    { steps = 4
    , persistence = 2
    , scale = 1
    }


fractal2d : FractalConfig -> PermutationTable -> Float -> Float -> Float
fractal2d { steps, persistence, scale } table x y =
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
                ( noise + (amp * Simplex.noise2d table (x / freq / scale) (y / freq / scale))
                , max + amp * 0.7
                )
            )
            ( 0, 0 )
        |> (\( noise, max ) -> noise / max)


type alias Neighbors a =
    { topLeft : a
    , top : a
    , topRight : a
    , left : a
    , center : a
    , right : a
    , bottomLeft : a
    , bottom : a
    , bottomRight : a
    }


getNeighbors : Int -> Int -> Matrix a -> Maybe (Neighbors a)
getNeighbors x y matrix =
    Just Neighbors
        |> Maybe.andMap (Matrix.get (x - 1) (y + 1) matrix)
        |> Maybe.andMap (Matrix.get x (y + 1) matrix)
        |> Maybe.andMap (Matrix.get (x + 1) (y + 1) matrix)
        |> Maybe.andMap (Matrix.get (x - 1) y matrix)
        |> Maybe.andMap (Matrix.get x y matrix)
        |> Maybe.andMap (Matrix.get (x + 1) y matrix)
        |> Maybe.andMap (Matrix.get (x - 1) (y - 1) matrix)
        |> Maybe.andMap (Matrix.get x (y - 1) matrix)
        |> Maybe.andMap (Matrix.get (x + 1) (y - 1) matrix)
