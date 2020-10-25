module Minimap exposing (..)

import Chunk exposing (Chunk)
import Dict exposing (Dict)
import Image
import Matrix exposing (Matrix)
import Playground
import Terrain exposing (Terrain(..))


render : Dict ( Int, Int ) (Maybe Chunk) -> Playground.Shape
render chunks =
    chunks
        |> Dict.toList
        |> List.filterMap
            (\( ( x, y ), chunk ) ->
                Maybe.map
                    (\chunk_ ->
                        Playground.image 64 64 chunk_.minimap
                            |> Playground.move
                                (toFloat x * 64)
                                (toFloat y * 64)
                    )
                    chunk
            )
        |> Playground.group


chunkImage : Matrix Terrain -> String
chunkImage =
    Matrix.map tileColor
        >> Matrix.transpose
        >> Matrix.toLists
        >> List.drop 1
        >> List.take 64
        >> List.map (List.drop 1 >> List.take 64)
        >> List.reverse
        >> Image.fromList2d
        >> Image.toPngUrl


tileColor : Terrain -> number
tileColor t =
    case t of
        Water ->
            0x0F3C6CFF

        Beach ->
            0xF7D2A0FF

        Dirt ->
            0xBE9052FF

        DirtDark ->
            0x784730FF

        Grass ->
            0x2F8136FF

        GrassDark ->
            0x004337FF

        GrassDry ->
            0x914E2FFF

        Snow ->
            0xE8EEF4FF

        Volcanic ->
            0x554C51FF

        Magma ->
            0xFF6A40FF

        Pond ->
            0x487784FF
