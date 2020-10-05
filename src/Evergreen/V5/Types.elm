module Evergreen.V5.Types exposing (..)

import Character exposing (Character)
import Dict exposing (Dict)
import Hash exposing (Hash)
import Lamdera exposing (ClientId)
import Playground
import World exposing (Chunk)


type alias FrontendModel =
    { page : Page
    }


type Page
    = StartPage
    | LoginPage LoginModel
    | RegisterPage RegisterModel
    | GamePage (Playground.Game Memory)


type alias Memory =
    { player : Character
    , others : Dict String Character
    , chunks : Dict ( Int, Int ) (Request Chunk)
    }


type Request a
    = Pending
    | Received a


type alias BackendModel =
    { accounts : List Account
    , chunks : Dict ( Int, Int ) Chunk
    }


type alias Account =
    { username : String
    , passwordHash : Hash
    , character : Character
    , loggedIn : Maybe ClientId
    }


type FrontendMsg
    = LoginMsg LoginMsg
    | RegisterMsg RegisterMsg
    | GameMsg Playground.Msg
    | GotoLogin
    | GotoRegister
    | Noop


type ToBackend
    = CheckName String
    | CreateAccount String Hash Int
    | Login String Hash
    | UpdatePlayer Character
    | GetChunk Int Int


type BackendMsg
    = BNoop


type ToFrontend
    = LoggedIn Account (Dict String Character)
    | CheckNameResponse Bool
    | WrongUsernameOrPassword
    | UsernameAlreadyExists
    | UpdateOtherPlayer String Character
    | ChunkResponse Int Int Chunk


type alias RegisterModel =
    { username : String
    , password : String
    , password2 : String
    , characterPicker : Bool
    , character : Maybe Int
    , failed : Bool
    , blurred : Bool
    }


type RegisterMsg
    = InputUsername String
    | InputPassword String
    | InputPassword2 String
    | Blurred
    | Next
    | SelectedCharacter Int
    | Register


type alias LoginModel =
    { username : String
    , password : String
    , failed : Bool
    }


type LoginMsg
    = LoginUsername String
    | LoginPassword String
    | Submit
