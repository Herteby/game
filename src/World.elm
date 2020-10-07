module World exposing (..)

import Image
import List.Extra as List
import Matrix exposing (Matrix)
import Maybe.Extra as Maybe
import Minimap
import Object
import Playground
import Random exposing (Seed)
import Simplex exposing (PermutationTable)
import Terrain exposing (..)
import Types exposing (..)


chunkSize =
    64


generateChunk : Int -> Int -> Chunk
generateChunk x y =
    let
        terrain =
            List.range (x * chunkSize) (x * chunkSize + chunkSize + 1)
                |> List.map
                    (\x_ ->
                        List.range (y * chunkSize) (y * chunkSize + chunkSize + 1)
                            |> List.map
                                (\y_ ->
                                    let
                                        env =
                                            environmentFromCoords x_ y_
                                    in
                                    ( generate env, env )
                                )
                    )

        matrix =
            terrain
                |> Matrix.fromLists
                |> Maybe.withDefault Matrix.empty

        matrix2 =
            Matrix.map Tuple.first matrix
    in
    { textures = List.map (\t -> ( t, tilemap matrix t )) Terrain.list
    , terrain = matrix2
    , objects =
        terrain
            |> List.map
                (List.map
                    (\( t, env ) ->
                        Random.step
                            (Object.generator t env)
                            env.seed
                            |> Tuple.first
                    )
                )
    , minimap = Minimap.chunkImage matrix2
    }


render : Chunk -> Playground.Shape
render chunk =
    let
        textures =
            List.map Terrain.render chunk.textures

        objects =
            chunk.objects
                |> List.indexedMap
                    (\x ->
                        List.indexedMap
                            (\y ->
                                Maybe.map
                                    (Object.render
                                        >> Playground.move (toFloat x * tileSize) (toFloat y * tileSize)
                                        >> Playground.move ((chunkSize / -2) * tileSize) ((chunkSize / -2) * tileSize)
                                    )
                            )
                            >> List.reverse
                    )
                |> List.transpose
                |> List.concat
                |> List.filterMap identity
    in
    textures
        ++ objects
        |> Playground.group


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42


permTable2 : PermutationTable
permTable2 =
    Simplex.permutationTableFromInt 420


environmentFromCoords : Int -> Int -> Environment
environmentFromCoords x y =
    let
        ( fx, fy ) =
            ( toFloat x, toFloat y )
    in
    { height = Simplex.fractal2d { steps = 8, stepSize = 2, persistence = 2, scale = 4 } permTable fx fy + 0.2
    , temp = Simplex.fractal2d { steps = 5, stepSize = 4, persistence = 2, scale = 10 } permTable2 fx fy
    , humidity = Simplex.fractal2d { steps = 5, stepSize = 4, persistence = 2, scale = 10 } permTable fx fy
    , foliage = Simplex.fractal2d { steps = 3, stepSize = 8, persistence = 2, scale = 2 } permTable fx fy
    , volcanism = Simplex.fractal2d { steps = 6, stepSize = 4, persistence = 2, scale = 2 } permTable2 fx fy
    , seed = Random.initialSeed (x * chunkSize * chunkSize + y)
    , x = x
    , y = y
    }


tilemap : Matrix ( Terrain, Environment ) -> Terrain -> String
tilemap matrix t =
    List.range 2 (chunkSize + 1)
        |> List.reverse
        |> List.map
            (\y_ ->
                List.range 2 (chunkSize + 1)
                    |> List.map
                        (\x_ ->
                            case getNeighbors x_ y_ matrix of
                                Just neighbors ->
                                    if t == Beach || anyNeighbor (\( t2, _ ) -> t2 == t) neighbors then
                                        let
                                            bools =
                                                mapNeighbors
                                                    (\( t2, _ ) ->
                                                        (t2 == t)
                                                            || (if t == Beach || t == Water then
                                                                    t == Beach && t2 /= Water

                                                                else
                                                                    Terrain.level t < Terrain.level t2
                                                               )
                                                    )
                                                    neighbors

                                            tile =
                                                edges bools
                                        in
                                        if tile == 11 || tile == 1 then
                                            case Matrix.get x_ y_ matrix of
                                                Just ( _, { seed } ) ->
                                                    seed
                                                        |> Random.step
                                                            (if tile == 11 then
                                                                Random.weighted ( 4, 11 ) [ ( 1, 16 ), ( 1, 17 ), ( 1, 18 ) ]

                                                             else
                                                                Random.uniform 1 [ 4 ]
                                                            )
                                                        |> Tuple.first

                                                Nothing ->
                                                    tile

                                        else
                                            tile

                                    else
                                        0

                                Nothing ->
                                    0
                        )
            )
        |> Image.fromList2d
        |> Image.toPngUrl


edges : Neighbors Bool -> Int
edges { topLeft, top, topRight, left, center, right, bottomLeft, bottom, bottomRight } =
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


mapNeighbors : (a -> b) -> Neighbors a -> Neighbors b
mapNeighbors fn neighbors =
    { topLeft = fn neighbors.topLeft
    , top = fn neighbors.top
    , topRight = fn neighbors.topRight
    , left = fn neighbors.left
    , center = fn neighbors.center
    , right = fn neighbors.right
    , bottomLeft = fn neighbors.bottomLeft
    , bottom = fn neighbors.bottom
    , bottomRight = fn neighbors.bottomRight
    }


anyNeighbor : (a -> Bool) -> Neighbors a -> Bool
anyNeighbor predicate neighbors =
    List.any predicate
        [ neighbors.topLeft
        , neighbors.top
        , neighbors.topRight
        , neighbors.left
        , neighbors.center
        , neighbors.right
        , neighbors.bottomLeft
        , neighbors.bottom
        , neighbors.bottomRight
        ]


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
