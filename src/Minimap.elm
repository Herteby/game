module Minimap exposing (..)

import AssocList as Assoc
import Dict exposing (Dict)
import Image
import Playground
import Playground.Extra as Playground
import Terrain
import Types exposing (Chunk, Request(..), Terrain(..))
import World


render : Dict ( Int, Int ) (Request Chunk) -> Playground.Shape
render chunks =
    chunks
        |> Dict.toList
        |> List.filterMap
            (\( ( x, y ), chunk ) ->
                case chunk of
                    Pending ->
                        Nothing

                    Received chunk_ ->
                        renderPixels chunk_
                            |> Playground.move
                                (toFloat x * World.chunkSize)
                                (toFloat y * World.chunkSize)
                            |> Just
            )
        |> Playground.group


renderPixels : Chunk -> Playground.Shape
renderPixels chunk =
    chunk.textures
        |> List.map (\( t, map ) -> Playground.tilemap 1 1 (tileColor t) map)
        |> Playground.group


tileColor : Terrain -> String
tileColor t =
    Assoc.get t tileColors |> Maybe.withDefault magenta


magenta =
    Image.fromList 1 [ 0xFF00FFFF ] |> Image.toPngUrl


tileColors : Assoc.Dict Terrain String
tileColors =
    Terrain.list
        |> List.map
            (\t ->
                ( t
                , [ case t of
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
                            0x00FF6A40

                        Pond ->
                            0x00487784
                  ]
                    |> Image.fromList 1
                    |> Image.toPngUrl
                )
            )
        |> Assoc.fromList
