module Chunk exposing (..)

import Matrix exposing (Matrix)
import Object exposing (Object)
import Terrain exposing (Terrain)


type Request a
    = Pending
    | Done a


type alias Chunk =
    { textures : List ( Terrain, String )
    , terrain : Matrix Terrain
    , objects : List (List (Maybe Object))
    , minimap : String
    }
