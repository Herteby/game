module Evergreen.V19.Playground.Internal exposing (..)

import Dict
import Set
import WebGL.Texture


type alias Time = 
    { now : Int
    , delta : Int
    }


type alias TextureManager = 
    { done : (Dict.Dict String WebGL.Texture.Texture)
    , loading : (Set.Set String)
    }