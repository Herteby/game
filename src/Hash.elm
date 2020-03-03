module Hash exposing (Hash, fromString)

import Sha256


type Hash
    = Hash String


fromString : String -> Hash
fromString str =
    Hash <| Sha256.sha256 (str ++ "salty sea dogs!")
