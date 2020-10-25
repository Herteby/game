module Character exposing (..)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Playground exposing (Computer, Keyboard, Time)
import Playground.Extra as Playground
import Set


type alias Character =
    { coords : Vec2
    , direction : Direction
    , speed : Speed
    , skin : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


type Speed
    = Standing
    | Walking
    | Sprinting


create : Int -> Maybe Character
create skin =
    if List.member skin skinList then
        Just
            { coords = { x = 0, y = 0 }
            , direction = Down
            , speed = Standing
            , skin = skin
            }

    else
        Nothing


skinList : List Int
skinList =
    List.range 1 45


update : { a | keyboard : Keyboard, time : Time } -> Character -> Character
update { keyboard, time } player =
    let
        dir =
            toXY keyboard

        d =
            time.delta |> toFloat |> clamp 0 60

        speed_ =
            if keyboard.shift then
                speed * 3

            else
                speed
    in
    { player
        | coords =
            Vec2.scale (speed_ * d) dir
                |> Vec2.add player.coords
        , direction =
            if dir.y > 0 then
                Up

            else if dir.y < 0 then
                Down

            else if dir.x < 0 then
                Left

            else if dir.x > 0 then
                Right

            else
                player.direction
        , speed =
            if toXY keyboard == { x = 0, y = 0 } then
                Standing

            else if keyboard.shift then
                Sprinting

            else
                Walking
    }


interpolate : Time -> Character -> Vec2 -> Vec2
interpolate time char coords =
    if coords == char.coords then
        coords

    else
        let
            sprint =
                if char.speed == Sprinting then
                    3

                else
                    1

            d =
                time.delta |> toFloat |> clamp 0 60

            dir =
                Vec2.direction char.coords coords

            newCoords =
                Vec2.add (Vec2.scale (d * speed * sprint) dir) coords
        in
        if Vec2.distance newCoords char.coords < Vec2.distance coords char.coords then
            newCoords

        else
            char.coords


url : Int -> String
url skin =
    "/characters/" ++ String.fromInt skin ++ ".png"


tile : Int -> Int -> Playground.Shape
tile skin frame =
    Playground.tile 26 36 (url skin) frame


render : Playground.Time -> Character -> Playground.Shape
render time char =
    let
        mod =
            time.now |> modBy 500

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
            if char.speed == Standing then
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
    tile char.skin frame


speed =
    0.2


toXY : Keyboard -> Vec2
toXY keyboard =
    let
        x =
            (if keyboard.right || Set.member "KeyD" keyboard.keys then
                1

             else
                0
            )
                - (if keyboard.left || Set.member "KeyA" keyboard.keys then
                    1

                   else
                    0
                  )

        y =
            (if keyboard.up || Set.member "KeyW" keyboard.keys then
                1

             else
                0
            )
                - (if keyboard.down || Set.member "KeyS" keyboard.keys then
                    1

                   else
                    0
                  )
    in
    if x /= 0 && y /= 0 then
        { x = x / squareRootOfTwo, y = y / squareRootOfTwo }

    else
        { x = x, y = y }


squareRootOfTwo : Float
squareRootOfTwo =
    sqrt 2
