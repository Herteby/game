module Character exposing (..)

import AltMath.Vector2 as Vec2 exposing (Vec2)
import Playground exposing (Keyboard)
import Playground.Extra as Playground
import Set


type alias Character =
    { coords : Vec2
    , direction : Direction
    , moving : Bool
    , skin : Int
    }


create : Int -> Maybe Character
create skin =
    if List.member skin skinList then
        Just { coords = { x = 0, y = 0 }, direction = Down, moving = False, skin = skin }

    else
        Nothing


skinList : List Int
skinList =
    List.range 1 45


type Direction
    = Up
    | Down
    | Left
    | Right


update { keyboard, time } { player, chatInput } =
    if chatInput /= Nothing then
        player

    else
        let
            kb =
                toXY keyboard

            d =
                Playground.delta time |> toFloat |> clamp 0 60

            speed_ =
                if keyboard.shift then
                    speed * 3

                else
                    speed
        in
        { player
            | coords =
                Vec2.scale (speed_ * d) kb
                    |> Vec2.add player.coords
            , direction =
                if kb.y > 0 then
                    Up

                else if kb.y < 0 then
                    Down

                else if kb.x < 0 then
                    Left

                else if kb.x > 0 then
                    Right

                else
                    player.direction
            , moving = toXY keyboard /= { x = 0, y = 0 }
        }


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
    tile char.skin frame
        |> Playground.move char.coords.x char.coords.y


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
