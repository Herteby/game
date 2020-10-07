module Evergreen.Migrate.V18 exposing (..)

import Dict
import Evergreen.V13.Types as Old
import Evergreen.V18.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated
        ( { page = New.StartPage }
        , Cmd.none
        )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated
        ( { accounts =
                old.accounts
                    |> List.map
                        (\a ->
                            { username = a.username
                            , passwordHash =
                                case a.passwordHash of
                                    Old.Hash str ->
                                        New.Hash str
                            , character =
                                { coords = a.character.coords
                                , direction =
                                    case a.character.direction of
                                        Old.Up ->
                                            New.Up

                                        Old.Down ->
                                            New.Down

                                        Old.Left ->
                                            New.Left

                                        Old.Right ->
                                            New.Right
                                , speed =
                                    case a.character.speed of
                                        Old.Standing ->
                                            New.Standing

                                        Old.Walking ->
                                            New.Walking

                                        Old.Sprinting ->
                                            New.Sprinting
                                , skin = a.character.skin
                                }
                            , loggedIn = a.loggedIn
                            }
                        )
          , chunks = Dict.empty
          }
        , Cmd.none
        )


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgOldValueIgnored


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgOldValueIgnored
