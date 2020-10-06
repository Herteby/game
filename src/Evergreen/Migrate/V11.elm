module Evergreen.Migrate.V11 exposing (..)

import Dict
import Evergreen.V11.Types as New
import Evergreen.V9.Types as Old
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated
        ( { page = New.StartPage }
        , Cmd.none
        )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel { accounts } =
    ModelMigrated
        ( { accounts =
                accounts
                    |> List.map
                        (\a ->
                            { username = a.username
                            , passwordHash =
                                case a.passwordHash of
                                    Old.Hash str ->
                                        New.Hash str
                            , character =
                                { coords = { x = 0, y = 0 }
                                , direction = New.Down
                                , speed = New.Standing
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
    MsgOldValueIgnored


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgOldValueIgnored


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgOldValueIgnored
