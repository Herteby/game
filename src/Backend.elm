module Backend exposing (Model, app)

import Character
import Dict
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }


type alias Model =
    BackendModel


init : ( Model, Cmd BackendMsg )
init =
    ( { accounts = []
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        BNoop ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        CreateAccount username passwordHash variant ->
            if List.any (\a -> a.username == username) model.accounts then
                ( model, Lamdera.sendToFrontend clientId RegisterFailed )

            else
                case Character.create variant of
                    Just character ->
                        let
                            account =
                                { username = username
                                , passwordHash = passwordHash
                                , character = character
                                , loggedIn = Just clientId
                                }

                            others =
                                model.accounts
                                    |> List.filterMap
                                        (\a ->
                                            if a.loggedIn == Nothing then
                                                Nothing

                                            else
                                                Just ( a.username, a.character )
                                        )
                                    |> Dict.fromList
                        in
                        ( { model
                            | accounts = account :: model.accounts
                          }
                        , Lamdera.sendToFrontend clientId (LoggedIn account others)
                        )

                    Nothing ->
                        ( model, Cmd.none )

        Login username passwordHash ->
            let
                match a =
                    a.username == username && a.passwordHash == passwordHash
            in
            case List.find match model.accounts of
                Just account ->
                    let
                        account_ =
                            { account | loggedIn = Just clientId }

                        others =
                            model.accounts
                                |> List.filterMap
                                    (\a ->
                                        if a.loggedIn == Nothing || a.username == account.username then
                                            Nothing

                                        else
                                            Just ( a.username, a.character )
                                    )
                                |> Dict.fromList
                    in
                    ( { model | accounts = List.setIf match account_ model.accounts }
                    , Lamdera.sendToFrontend clientId (LoggedIn account_ others)
                    )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId LoginFailed )

        UpdatePlayer character ->
            case List.find (\a -> a.loggedIn == Just clientId) model.accounts of
                Just account ->
                    ( { model
                        | accounts =
                            List.setIf (\a -> a.loggedIn == Just clientId)
                                { account | character = character }
                                model.accounts
                      }
                    , model.accounts
                        |> List.filterMap .loggedIn
                        |> List.filter (\id -> id /= clientId)
                        |> List.map (\id -> Lamdera.sendToFrontend id (UpdateOtherPlayer account.username character))
                        |> Cmd.batch
                    )

                Nothing ->
                    ( model, Cmd.none )
