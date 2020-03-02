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


url variant =
    "/characters/" ++ String.fromInt variant ++ ".png"


tile variant =
    Playground.tile 26 36 (url variant)


anim time a =
    let
        mod =
            Playground.now time |> modBy 500
    in
    if mod < 125 then
        a

    else if mod < 250 then
        a + 1

    else if mod < 375 then
        a + 2

    else
        a + 1


render time char =
    tile char.variant <|
        case ( char.direction, char.moving ) of
            ( Down, False ) ->
                1

            ( Down, True ) ->
                anim time 0

            ( Left, False ) ->
                4

            ( Left, True ) ->
                anim time 3

            ( Right, False ) ->
                7

            ( Right, True ) ->
                anim time 6

            ( Up, False ) ->
                10

            ( Up, True ) ->
                anim time 9


speed =
    0.2
