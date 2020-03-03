module Types exposing (..)

import Character exposing (Character)
import Dict exposing (Dict)
import Hash exposing (Hash)
import Lamdera exposing (ClientId)
import Playground


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
    }


type alias BackendModel =
    { accounts : List Account
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
    = CreateAccount String Hash Int
    | Login String Hash
    | UpdatePlayer Character


type BackendMsg
    = BNoop


type ToFrontend
    = LoggedIn Account
    | LoginFailed
    | RegisterFailed
    | UpdateOtherPlayer String Character


type alias RegisterModel =
    { username : String
    , password : String
    , password2 : String
    , character : Maybe Int
    , failed : Bool
    }


type RegisterMsg
    = InputUsername String
    | InputPassword String
    | InputPassword2 String
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
