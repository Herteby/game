module Evergreen.V11.Character exposing (..)

import AltMath.Vector2


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Character = 
    { coords : AltMath.Vector2.Vec2
    , direction : Direction
    , moving : Bool
    , skin : Int
    }