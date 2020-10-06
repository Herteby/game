module Evergreen.V13.Playground.Internal exposing (..)

import Browser.Dom
import Browser.Events
import Dict
import Math.Vector2
import Set
import Time
import WebGL
import WebGL.Texture


type TextureData
    = Loading
    | Success 
    { texture : WebGL.Texture.Texture
    , size : Math.Vector2.Vec2
    }
    | Fail WebGL.Texture.Error


type alias TextureManager = (Dict.Dict String TextureData)


type alias Number = Float


type alias Mouse = 
    { x : Number
    , y : Number
    , down : Bool
    , click : Bool
    }


type alias Keyboard = 
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    , space : Bool
    , enter : Bool
    , shift : Bool
    , backspace : Bool
    , keys : (Set.Set String)
    }


type alias Screen = 
    { width : Number
    , height : Number
    , top : Number
    , left : Number
    , right : Number
    , bottom : Number
    }


type Time
    = Time Time.Posix Int


type alias Computer = 
    { mouse : Mouse
    , keyboard : Keyboard
    , screen : Screen
    , time : Time
    }


type Game memory
    = Game 
    { visibility : Browser.Events.Visibility
    , memory : memory
    , textures : TextureManager
    , computer : Computer
    , entities : (List WebGL.Entity)
    }


type Msg
    = KeyChanged Bool String
    | Tick Time.Posix
    | GotViewport Browser.Dom.Viewport
    | Resized Screen
    | VisibilityChanged Browser.Events.Visibility
    | MouseMove Float Float
    | MouseClick
    | MouseButton Bool
    | GotTexture (Result (String, WebGL.Texture.Error) (String, WebGL.Texture.Texture))