module Playground.Advanced exposing
    ( custom, useTexture
    , Render, Opacity, ScaleRotateSkew, Translate
    , embed, get, edit, subscriptions, resize, updateTexture
    )

{-| Advanced user section, for:

1.  packages authors (if you like create addons)
2.  custom Shape creation
3.  embedding into other app


# Shape

@docs custom, useTexture


# Types

@docs Render, Opacity, ScaleRotateSkew, Translate


# Embedding

@docs embed, get, edit, subscriptions, resize, updateTexture

-}

import Dict
import Html exposing (Html)
import Math.Vector2 exposing (Vec2)
import Math.Vector4 exposing (Vec4)
import Playground.Internal as Internal exposing (Form(..), Game(..), Number, Shape(..), TextureData(..))
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)


{-| Create your own shape

    redRect w h =
        (\uP uT opacity ->
            WebGL.entity
                rectVertexShader
                fillFragment
                clipPlate
                { color = Math.Vector4.vec4 1 0 0 opacity
                , uP = uP
                , uT = uT
                }
        )
            |> custom w h

-}
custom : Number -> Number -> Render -> Shape
custom width height render =
    Shape { x = 0, y = 0, a = 0, sx = 1, sy = 1, o = 1, form = Form width height render }


{-| Get texture for your custom Shape

    useTexture "image.png" <|
        \t ->
            custom 32 32 <|
                myCustomRender t

-}
useTexture : String -> (Texture.Texture -> Shape) -> Shape
useTexture url fn =
    Shape { x = 0, y = 0, a = 0, sx = 1, sy = 1, o = 1, form = Textured url fn }



------ Embed Stuff ------


{-| When you need advanced application or just need to playground be part of your app
-}
embed :
    (Internal.Computer -> memory -> List Shape)
    -> (Internal.Computer -> memory -> memory)
    -> memory
    ->
        { init : ( Internal.Game memory, Cmd Internal.Msg )
        , view : Internal.Game memory -> Html a
        , update : Internal.Msg -> Internal.Game memory -> ( Internal.Game memory, Cmd Internal.Msg )
        }
embed viewMemory updateMemory initialMemory =
    let
        { init, update } =
            Internal.create viewMemory updateMemory initialMemory

        view (Game { computer, entities }) =
            Internal.embedViewWrap computer.screen entities
    in
    { init = init
    , view = view
    , update = update
    }


{-| Modify game memory from the outside world
-}
edit : (Internal.Computer -> memory -> memory) -> Game memory -> Game memory
edit fn (Game ({ memory, computer } as model)) =
    Game { model | memory = fn computer memory }


{-| Expose internals to the outside wold
-}
get : Game memory -> ( Internal.Computer, memory )
get (Game { computer, memory }) =
    ( computer, memory )


{-| Playground have different subscriptions:

  - `picture` uses only `subscriptions.resize`
  - `animation` uses only `subscriptions.resize` and `subscriptions.time`
  - `game` uses all (shortcut `subscriptions.all`)

When embedding playground you can choose what to use:

  - `keys` - listen to keyboard events (keyUp and keyDown) and updates
  - `time` - updates `Computer.time` each frame
  - `click` - listen on mouse movement `Computer.mouse.down/click`
  - `mouse` - listen on mouse movement `Computer.mouse.x/y`
  - `resize` - listen on window resize and updates `Computer.screen`

-}
subscriptions :
    { keys : Sub Internal.Msg
    , time : Sub Internal.Msg
    , click : Sub Internal.Msg
    , mouse : Sub Internal.Msg
    , resize : Sub Internal.Msg
    , all : Sub Internal.Msg
    }
subscriptions =
    Internal.subscriptions


{-| Embedded target is be part of your application,
most of time it will not take whole screen,
so it is up to you decide on dimensions, and pass it to update function as Msg
-}
resize : Int -> Int -> Internal.Msg
resize =
    Internal.resize


{-| All textures start loading only after first time request in `view`,
with slow network that can take some time,
but for more advanced games and more smooth transition you can cache textures for later use,
so when you need show new image - texture already there.

`updateTexture` gives you control over textures stored in `Game`

    newModel =
        model
            --add new texture
            |> updateTexture "img1.png" (\_ -> Just myTexture)
            --remove texture
            |> updateTexture "img2.png" (\_ -> Nothing)

-}
updateTexture : String -> (Maybe Texture -> Maybe Texture) -> Game memory -> Game memory
updateTexture key fn (Game ({ textures } as model)) =
    Game
        { model
            | textures =
                Dict.update key
                    (\v ->
                        (case v of
                            Just (Success { texture }) ->
                                fn (Just texture)

                            _ ->
                                fn Nothing
                        )
                            |> Maybe.map
                                (\t ->
                                    Success
                                        { texture = t
                                        , size =
                                            Texture.size t
                                                |> (\( w, h ) -> Math.Vector2.vec2 (toFloat w) (toFloat h))
                                        }
                                )
                    )
                    textures
        }


{-| Create Your own Render and use it in [`Advanced.custom`](Playground-Advanced#custom) to create [`Shape`](Playground#Shape)

    redRect : Render
    redRect uP uT opacity =
        WebGL.entityWith
            defaultEntitySettings
            vertNone
            fragFill
            mesh
            { color = setAlpha color opacity
            , uP = uP
            , uT = uT
            }

-}
type alias Render =
    Translate
    -> ScaleRotateSkew
    -> Opacity
    -> WebGL.Entity


{-| Vec2 representing part of transform matrix for [`Advanced.custom`](Playground-Advanced#custom)

    | 1 0 x |
    | 0 1 y |
    | 0 0 1 |

-}
type alias Translate =
    Vec2


{-| Vec4 representing part of transform matrix for [`Advanced.custom`](Playground-Advanced#custom)

    | x y 0 |
    | z w 0 |
    | 0 0 1 |

-}
type alias ScaleRotateSkew =
    Vec4


{-| -}
type alias Opacity =
    Float
