module Hash exposing (fromString)

import Sha256
import Types exposing (Hash(..))


fromString : String -> Hash
fromString str =
    Hash <| Sha256.sha256 (str ++ "salty sea dogs!")
