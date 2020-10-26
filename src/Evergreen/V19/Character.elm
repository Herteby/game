module Evergreen.V19.Character exposing (..)

import AltMath.Vector2


type Direction
    = Up
    | Down
    | Left
    | Right


type Speed
    = Standing
    | Walking
    | Sprinting


type alias Character = 
    { coords : AltMath.Vector2.Vec2
    , direction : Direction
    , speed : Speed
    , skin : Int
    }