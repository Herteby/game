module World exposing (..)

import Image
import Matrix exposing (Matrix)
import Maybe.Extra as Maybe
import Playground
import Playground.Extra as Playground
import Simplex exposing (PermutationTable)


type alias Chunk =
    { textures : List ( Terrain, String )
    }


type Object
    = Object


generateChunk : Int -> Int -> Chunk
generateChunk x y =
    let
        matrix =
            List.range (x * 64) (x * 64 + 66)
                |> List.map
                    (\x_ ->
                        List.range (y * 64) (y * 64 + 66)
                            |> List.map
                                (\y_ ->
                                    valuesFromCoord x_ y_
                                        |> terrainFromValues
                                )
                    )
                |> Matrix.fromLists
                |> Maybe.withDefault Matrix.empty

        _ =
            Debug.log "size" (Matrix.size matrix)
    in
    { textures =
        [ ( Water, image matrix Water )
        , ( Beach, image matrix Beach )
        , ( Dirt, image matrix Dirt )
        , ( Grass, image matrix Grass )
        ]
    }


render : Chunk -> Playground.Shape
render chunk =
    chunk.textures
        |> List.map (\( terrain, img ) -> texture terrain img)
        |> Playground.group


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42


texture : Terrain -> String -> Playground.Shape
texture t =
    let
        str =
            case t of
                Beach ->
                    "beach"

                Grass ->
                    "grass"

                Dirt ->
                    "dirt1"

                Water ->
                    "waterDeep"
    in
    Playground.tilemap 32 32 <| "/terrain/" ++ str ++ ".png"


type Terrain
    = Beach
    | Grass
    | Dirt
    | Water


valuesFromCoord : Int -> Int -> { height : Float, temp : Float, humidity : Float, random : Float }
valuesFromCoord x y =
    let
        ( fx, fy ) =
            ( toFloat x, toFloat y )
    in
    { height = fractal2d { steps = 6, persistence = 2, scale = 4 } permTable fx fy
    , temp = fractal2d { steps = 1, persistence = 2, scale = 20 } permTable fx fy
    , humidity = fractal2d { steps = 1, persistence = 2, scale = 19 } permTable fx fy
    , random = Simplex.noise2d permTable fx fy
    }


terrainFromValues : { height : Float, temp : Float, humidity : Float, random : Float } -> ( Terrain, Float )
terrainFromValues { height, temp, humidity, random } =
    ( if height > 0.2 then
        Grass

      else if height > 0.15 then
        Dirt

      else if height > 0 then
        Beach

      else
        Water
    , random
    )


image : Matrix ( Terrain, Float ) -> Terrain -> String
image matrix t =
    let
        bools =
            Matrix.map
                (\( t2, _ ) ->
                    t
                        == t2
                        || (t == Beach && t2 /= Water)
                        || (t == Dirt && t2 == Grass)
                )
                matrix

        _ =
            Debug.log "bools" (Matrix.size bools)
    in
    List.range 2 65
        |> List.reverse
        |> List.map
            (\y_ ->
                List.range 2 65
                    |> List.map
                        (\x_ ->
                            case getNeighbors x_ y_ bools of
                                Just n ->
                                    let
                                        tile =
                                            edges n
                                    in
                                    if tile == 11 then
                                        case Matrix.get x_ y_ matrix of
                                            Just ( _, random ) ->
                                                if random < 0 then
                                                    11

                                                else if random < 0.25 then
                                                    16

                                                else if random < 0.5 then
                                                    17

                                                else
                                                    18

                                            Nothing ->
                                                tile

                                    else
                                        tile

                                Nothing ->
                                    {- let
                                           _ =
                                               Debug.log "ERROR" ( x_, y_ )
                                       in
                                    -}
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


fractal2d : { steps : Int, persistence : Float, scale : Float } -> PermutationTable -> Float -> Float -> Float
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
