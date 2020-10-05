module Object exposing (..)

import Array exposing (Array)
import Playground exposing (Shape)
import Playground.Extra as Playground
import Random exposing (Generator)
import Terrain
import Types exposing (..)


generator : Terrain -> Environment -> Generator (Maybe Object)
generator terrain ({ height, humidity, temp, foliage, x, y } as env) =
    let
        grassStuff =
            if
                (height > 0.14)
                    && (foliage > 0.3)
                    && (modBy 6 (x + y) == 0)
                    && (modBy 3 y == 0)
            then
                if temp < 0 then
                    Random.uniform C1 [ C2, C3, C4, C5 ]
                        |> Random.map (Conifer NoSnow >> Just)

                else if humidity < -0.3 then
                    percent
                        [ ( 25
                          , Random.uniform D1 [ D2, D3, D4, D5 ]
                                |> Random.map DeadTree
                          )
                        ]

                else if humidity < -0.2 then
                    percent
                        [ ( 25
                          , Random.uniform D1 [ D2, D3, D4, D5 ]
                                |> Random.map DeadTree
                          )
                        , ( 25, randomTree env )
                        ]

                else if temp < 0.1 then
                    Random.uniform (randomTree env)
                        [ Random.uniform C1 [ C2, C3, C4, C5 ]
                            |> Random.map (Conifer NoSnow)
                        ]
                        |> Random.andThen (Random.map Just)

                else
                    Random.map Just <| randomTree env

            else if foliage < -0.4 && modBy 2 x == 0 && modBy 2 y == 0 then
                percent
                    [ ( 30, randomFlower env ) ]

            else
                none
    in
    case terrain of
        Water ->
            none

        Beach ->
            if height < 0.03 then
                percent
                    [ ( 10, Random.int 0 27 |> Random.map Shell ) ]

            else
                none

        Dirt ->
            none

        DirtDark ->
            none

        Grass ->
            grassStuff

        GrassDark ->
            grassStuff

        GrassDry ->
            grassStuff

        Snow ->
            if
                (height > 0.14)
                    && (foliage > 0.3)
                    && (modBy 5 (x + y) == 0)
                    && (modBy 3 y == 0)
            then
                Random.uniform C1 [ C2, C3, C4, C5 ]
                    |> Random.map (Conifer WithSnow >> Just)

            else
                none

        Volcanic ->
            none

        Magma ->
            none

        Pond ->
            none


randomTree : Environment -> Generator Object
randomTree { temp } =
    Random.map2 Tree
        (if temp > 0.3 then
            Random.constant Green

         else if temp > 0.2 then
            Random.uniform Green [ Yellow ]

         else if temp > 0.1 then
            Random.uniform Yellow [ Orange ]

         else
            Random.constant Orange
        )
        (Random.uniform T1 [ T2, T3, T4 ])


randomFlower : Environment -> Generator Object
randomFlower { temp, humidity, volcanism, x, y } =
    let
        color =
            case ( temp > 0, humidity > 0, volcanism > 0 ) of
                ( True, True, _ ) ->
                    FlowerRed

                ( True, False, _ ) ->
                    FlowerYellow

                ( False, False, _ ) ->
                    FlowerBlue

                ( False, True, True ) ->
                    FlowerPurple

                ( False, True, False ) ->
                    FlowerPink
    in
    Random.map (Flower color) (Random.uniform F1 [ F2, F3, F4 ])


percent : List ( Float, Generator a ) -> Generator (Maybe a)
percent l =
    let
        total =
            l |> List.map Tuple.first |> List.sum
    in
    Random.weighted ( 100 - total, none )
        (List.map (Tuple.mapSecond (Random.map Just)) l)
        |> Random.andThen identity


none : Generator (Maybe a)
none =
    Random.constant Nothing


render : Object -> Shape
render object =
    case object of
        Tree color variant ->
            renderTree color variant

        Conifer variant snow ->
            renderConifer variant snow

        DeadTree variant ->
            renderDeadTree variant

        Shell variant ->
            renderShell variant

        Flower color variant ->
            renderFlower color variant


renderTree : TreeColor -> TreeVariant -> Shape
renderTree color variant =
    let
        sprite_ =
            case variant of
                T1 ->
                    { xmin = 0, xmax = 3, ymin = 0, ymax = 4 }

                T2 ->
                    { xmin = 3, xmax = 6, ymin = 0, ymax = 4 }

                T3 ->
                    { xmin = 6, xmax = 9, ymin = 0, ymax = 4 }

                T4 ->
                    { xmin = 9, xmax = 12, ymin = 0, ymax = 4 }

        sprite =
            case color of
                Green ->
                    sprite_

                Yellow ->
                    { sprite_ | ymin = 4, ymax = 8 }

                Orange ->
                    { sprite_ | ymin = 8, ymax = 12 }

        coords =
            { xmin = sprite.xmin * 32
            , xmax = sprite.xmax * 32
            , ymin = sprite.ymin * 32
            , ymax = sprite.ymax * 32
            }
    in
    Playground.sprite "/objects/trees.png" coords
        |> Playground.moveUp ((sprite.ymax - sprite.ymin - 2) * 32)


renderConifer : ConiferSnow -> ConiferVariant -> Shape
renderConifer snow variant =
    let
        sprite_ =
            case variant of
                C1 ->
                    { xmin = 0, xmax = 3, ymin = 0, ymax = 5 }

                C2 ->
                    { xmin = 3, xmax = 6, ymin = 0, ymax = 5 }

                C3 ->
                    { xmin = 6, xmax = 9, ymin = 0, ymax = 5 }

                C4 ->
                    { xmin = 9, xmax = 12, ymin = 0, ymax = 5 }

                C5 ->
                    { xmin = 12, xmax = 15, ymin = 0, ymax = 5 }

        sprite =
            case snow of
                NoSnow ->
                    sprite_

                WithSnow ->
                    { sprite_ | ymin = 5, ymax = 10 }

        coords =
            { xmin = sprite.xmin * 32
            , xmax = sprite.xmax * 32
            , ymin = sprite.ymin * 32
            , ymax = sprite.ymax * 32
            }
    in
    Playground.sprite "/objects/conifers.png" coords
        |> Playground.moveUp ((sprite.ymax - sprite.ymin - 2) * 32)


renderDeadTree : DeadTreeVariant -> Shape
renderDeadTree variant =
    Playground.tile 96 128 "/objects/deadTrees.png" <|
        case variant of
            D1 ->
                0

            D2 ->
                1

            D3 ->
                2

            D4 ->
                3

            D5 ->
                4


renderShell : Int -> Shape
renderShell variant =
    Playground.tile 32 32 "/objects/shells.png" variant
        |> Playground.scale 0.5


renderFlower : FlowerColor -> FlowerVariant -> Shape
renderFlower color variant =
    let
        row =
            case color of
                FlowerRed ->
                    0

                FlowerYellow ->
                    1

                FlowerBlue ->
                    2

                FlowerPurple ->
                    3

                FlowerPink ->
                    4

        column =
            case variant of
                F1 ->
                    0

                F2 ->
                    1

                F3 ->
                    2

                F4 ->
                    3
    in
    Playground.tile 32 32 "/objects/flowers.png" (row * 4 + column)
