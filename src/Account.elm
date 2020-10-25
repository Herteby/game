module Account exposing (..)

import Character exposing (Character)
import Hash exposing (Hash)
import Lamdera exposing (ClientId)


type alias Account =
    { username : String
    , passwordHash : Hash
    , character : Character
    , loggedIn : Maybe ClientId
    }
