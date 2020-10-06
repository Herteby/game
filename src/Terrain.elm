module Terrain exposing (..)

import Playground
import Playground.Extra as Playground
import Types exposing (..)


tileSize =
    16


generate : Environment -> Terrain
generate { height, temp, humidity, volcanism } =
    if height < 0 then
        Water

    else if volcanism > 0.45 && height > 0.1 then
        Magma

    else if volcanism > 0.3 then
        Volcanic

    else if volcanism > 0.29 && height > 0.12 then
        DirtDark

    else if temp < -0.2 then
        Snow

    else if height < 0.1 then
        Beach

    else if height < 0.12 then
        Dirt

    else if humidity > 0.5 then
        Pond

    else if humidity > 0.3 then
        GrassDark

    else if humidity < -0.2 || humidity + temp < -0.1 then
        GrassDry

    else
        Grass


list : List Terrain
list =
    [ Water
    , Beach
    , Dirt
    , DirtDark
    , GrassDry
    , Grass
    , GrassDark
    , Snow
    , Volcanic
    , Magma
    , Pond
    ]


level : Terrain -> Int
level terrain =
    recurse terrain 1 list


recurse terrain i l =
    case l of
        t :: rest ->
            if t == terrain then
                i

            else
                recurse terrain (i + 1) rest

        [] ->
            0


render : ( Terrain, String ) -> Playground.Shape
render ( t, map ) =
    let
        str =
            case t of
                Water ->
                    "waterDeep"

                Beach ->
                    "beach"

                Dirt ->
                    "dirt1"

                DirtDark ->
                    "dirt3"

                Grass ->
                    "grass"

                GrassDark ->
                    "grassDark"

                GrassDry ->
                    "grassDry"

                Snow ->
                    "snow"

                Volcanic ->
                    "volcanic"

                Magma ->
                    "magma"

                Pond ->
                    "pond"
    in
    Playground.tilemap tileSize tileSize ("/terrain/" ++ str ++ ".png") map
