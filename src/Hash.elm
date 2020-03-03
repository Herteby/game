module Hash exposing (Hash, fromString)

import Sha256


type Hash
    = Hash String


fromString : String -> Hash
fromString =
    Sha256.sha256 >> Hash
