module Evergreen.V19.Object exposing (..)

type TreeColor
    = Green
    | Yellow
    | Orange


type TreeVariant
    = T1
    | T2
    | T3
    | T4


type ConiferSnow
    = NoSnow
    | WithSnow


type ConiferVariant
    = C1
    | C2
    | C3
    | C4
    | C5


type DeadTreeVariant
    = D1
    | D2
    | D3
    | D4
    | D5


type FlowerColor
    = FlowerRed
    | FlowerYellow
    | FlowerBlue
    | FlowerPurple
    | FlowerPink


type FlowerVariant
    = F1
    | F2
    | F3
    | F4


type Object
    = Tree TreeColor TreeVariant
    | Conifer ConiferSnow ConiferVariant
    | DeadTree DeadTreeVariant
    | Shell Int
    | Flower FlowerColor FlowerVariant