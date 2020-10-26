module Evergreen.V19.Chunk exposing (..)

import Matrix
import Evergreen.V19.Object
import Evergreen.V19.Terrain


type alias Chunk = 
    { textures : (List (Evergreen.V19.Terrain.Terrain, String))
    , terrain : (Matrix.Matrix Evergreen.V19.Terrain.Terrain)
    , objects : (List (List (Maybe Evergreen.V19.Object.Object)))
    , minimap : String
    }


type Request a
    = Pending
    | Done a