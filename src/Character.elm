module Character exposing (..)

import Playground
import Playground.Extra as Playground


type alias Character =
    { coords : ( Float, Float )
    , direction : Direction
    , moving : Bool
    , variant : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


update { keyboard, time } character =
    let
        ( x, y ) =
            character.coords

        d =
            Playground.delta time |> toFloat
    in
    { character
        | coords =
            ( x + (Playground.toX keyboard * speed * d)
            , y + (Playground.toY keyboard * speed * d)
            )
        , direction =
            if keyboard.up then
                Up

            else if keyboard.down then
                Down

            else if keyboard.left then
                Left

            else if keyboard.right then
                Right

            else
                character.direction
        , moving = keyboard.up || keyboard.down || keyboard.left || keyboard.right
    }


url : Int -> String
url variant =
    "/characters/" ++ String.fromInt variant ++ ".png"


tile : Int -> Int -> Playground.Shape
tile variant =
    Playground.tile 26 36 (url variant)


render : Playground.Time -> Character -> Playground.Shape
render time char =
    let
        mod =
            Playground.now time |> modBy 500

        row =
            case char.direction of
                Down ->
                    0

                Left ->
                    3

                Right ->
                    6

                Up ->
                    9

        frame =
            if not char.moving then
                row + 1

            else if mod < 125 then
                row

            else if mod < 250 then
                row + 1

            else if mod < 375 then
                row + 2

            else
                row + 1
    in
    tile char.variant frame


speed =
    0.2
