module Evergreen.V19.Account exposing (..)

import Evergreen.V19.Character
import Evergreen.V19.Hash
import Lamdera


type alias Account = 
    { username : String
    , passwordHash : Evergreen.V19.Hash.Hash
    , character : Evergreen.V19.Character.Character
    , loggedIn : (Maybe Lamdera.ClientId)
    }