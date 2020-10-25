module GamePage exposing (..)

import Account exposing (Account)
import AltMath.Vector2 as Vec2 exposing (Vec2)
import Browser.Dom
import Browser.Events exposing (Visibility(..))
import Character exposing (Character, Direction(..), Speed(..))
import Chunk exposing (Chunk)
import Dict exposing (Dict)
import FontAwesome.Icon exposing (Icon)
import FontAwesome.Solid as Solid
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Html.Lazy exposing (..)
import Json.Decode as Decode
import Keyboard.Key as Key
import Lamdera
import Minimap
import Playground exposing (Computer, Keyboard, Screen, Shape)
import Playground.Internal exposing (TextureManager)
import Set exposing (Set)
import Task
import Terrain
import Time
import Types exposing (..)
import UI exposing (..)
import UI.Icon as Icon
import WebGL
import WebGL.Shape2d
import WebGL.Texture as Texture exposing (Texture)
import World


init : Account -> Dict String Character -> Time.Posix -> ( GameModel, Cmd GameMsg )
init account others posix =
    ( { time = { now = Time.posixToMillis posix, delta = 0 }
      , screen = toScreen 600 600
      , visibility = Visible
      , keyboard = []
      , textures = { done = Dict.empty, loading = Set.empty }
      , entities = []
      , player = account.character
      , others = Dict.map (\_ c -> ( c, c.coords )) others
      , chunks = Dict.empty
      , messages = []
      , chatInput = Nothing
      , messageI = 0
      , showPlayerList = False
      , lastUpdate = ( 0, account.character )
      , fps = []
      , showMinimap = False
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )


subscriptions : GameModel -> Sub GameMsg
subscriptions model =
    case model.visibility of
        Hidden ->
            Browser.Events.onVisibilityChange VisibilityChanged

        Visible ->
            Sub.batch
                [ Browser.Events.onKeyUp (Decode.field "keyCode" Decode.int |> Decode.map (Key.fromCode >> KeyUp))
                , Browser.Events.onKeyDown
                    (Decode.field "repeat" Decode.bool
                        |> Decode.andThen
                            (\repeat ->
                                if repeat then
                                    Decode.fail ""

                                else
                                    Decode.field "keyCode" Decode.int
                                        |> Decode.map (Key.fromCode >> KeyDown)
                            )
                    )
                , Browser.Events.onAnimationFrame Tick
                , Browser.Events.onVisibilityChange VisibilityChanged
                , Browser.Events.onResize (\w h -> Resized <| toScreen (toFloat w) (toFloat h))
                ]


update : GameMsg -> GameModel -> ( GameModel, Cmd GameMsg )
update msg model =
    case msg of
        KeyDown key ->
            if key == Key.Enter then
                case model.chatInput of
                    Just message ->
                        ( { model | chatInput = Nothing, keyboard = [] }
                        , if message == "" then
                            Cmd.none

                          else
                            Lamdera.sendToBackend (SendMessage message)
                        )

                    Nothing ->
                        ( { model | chatInput = Just "", keyboard = [] }
                        , Browser.Dom.focus "chatInput" |> Task.attempt (always Noop2)
                        )

            else
                ( { model | keyboard = key :: model.keyboard }, Cmd.none )

        KeyUp key ->
            ( { model | keyboard = List.filter ((/=) key) model.keyboard }, Cmd.none )

        GotViewport { viewport } ->
            ( { model | screen = toScreen viewport.width viewport.height }
            , Cmd.none
            )

        Resized newScreen ->
            ( { model | screen = newScreen }
            , Cmd.none
            )

        VisibilityChanged vis ->
            ( { model
                | time = { now = model.time.now, delta = 0 }
                , keyboard = []
              }
            , Cmd.none
            )

        GotTexture r ->
            ( { model
                | textures = gotTextures r model.textures
              }
            , Cmd.none
            )

        Tick posix ->
            let
                now =
                    Time.posixToMillis posix

                delta =
                    now - model.time.now

                ( cx, cy ) =
                    model.player.coords
                        |> (\{ x, y } ->
                                ( round <| x / (64 * 16) - 0.5, round <| y / (64 * 16) - 0.5 )
                           )

                ( lastUpdate, cmd_ ) =
                    if model.player /= Tuple.second model.lastUpdate && (model.time.now > Tuple.first model.lastUpdate + 250) then
                        ( ( model.time.now, model.player )
                        , Lamdera.sendToBackend (UpdatePlayer model.player)
                        )

                    else
                        ( model.lastUpdate, Cmd.none )

                ( entities, missing ) =
                    WebGL.Shape2d.toEntities model.textures.done model.screen (render model)

                ( textures, cmd__ ) =
                    requestTexture missing model.textures
            in
            ( { model
                | time = { now = now, delta = delta }
                , entities = entities
                , lastUpdate = lastUpdate
                , textures = textures
                , player =
                    if model.chatInput == Nothing then
                        Character.update model model.player

                    else
                        model.player
                , others = Dict.map (\_ ( c, coords ) -> ( c, Character.interpolate model.time c coords )) model.others
                , fps = 1000 // model.time.delta :: model.fps |> List.take 60
              }
            , Cmd.batch [ cmd_, cmd__ ]
            )
                |> andThen (checkChunk cx cy)
                |> andThen (checkChunk (cx + 1) cy)
                |> andThen (checkChunk cx (cy + 1))
                |> andThen (checkChunk (cx + 1) (cy + 1))

        ChatInput message ->
            ( { model | chatInput = Just message }, Cmd.none )

        ToggleMinimap ->
            ( { model | showMinimap = not model.showMinimap }, Cmd.none )

        TogglePlayerList ->
            ( { model | showPlayerList = not model.showPlayerList }, Cmd.none )

        RemoveMessage i ->
            ( { model | messages = List.filter (\( i_, m ) -> i_ /= i) model.messages }
            , Cmd.none
            )

        Noop2 ->
            ( model, Cmd.none )


andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen fn ( model, cmd ) =
    fn model |> Tuple.mapSecond (\cmd_ -> Cmd.batch [ cmd, cmd_ ])


checkChunk x y model =
    if Dict.get ( x, y ) model.chunks == Nothing then
        ( { model | chunks = Dict.insert ( x, y ) Nothing model.chunks }
        , Lamdera.sendToBackend (GetChunk x y)
        )

    else
        ( model, Cmd.none )


view : GameModel -> Html GameMsg
view game =
    div []
        [ WebGL.toHtmlWith webGLOption
            [ Attributes.width (round game.screen.width)
            , Attributes.height (round game.screen.height)
            ]
            game.entities
        , lazy2 chat game.messages game.chatInput
        , info game
        , namePlates game.player game.others
        , if game.showPlayerList then
            playerList game.others

          else
            none
        ]


webGLOption : List WebGL.Option
webGLOption =
    [ WebGL.alpha True
    , WebGL.depth 1
    , WebGL.clearColor 1 1 1 1
    ]


info : GameModel -> Html msg
info { fps, player } =
    div [ class "coords" ]
        [ div [] [ text <| "fps: " ++ (String.fromInt <| List.sum fps // List.length fps) ]
        , div [] [ text ("x: " ++ String.fromInt (round player.coords.x)) ]
        , div [] [ text ("y: " ++ String.fromInt (round player.coords.y)) ]
        ]


chat : List ( Int, Message ) -> Maybe String -> Html GameMsg
chat messages chatInput =
    div [ class "bottomLeft" ]
        [ div [ class "chat" ]
            [ messages
                |> List.reverse
                |> List.map
                    (\( i, m ) ->
                        ( String.fromInt i
                        , case m of
                            UserMessage { username, skin, message } ->
                                div [ class "message" ]
                                    [ div [ class "avatar", style "background-image" ("url(" ++ Character.url skin ++ ")") ] []
                                    , div []
                                        [ div [ class "username" ] [ text username ]
                                        , div [] [ text message ]
                                        ]
                                    ]

                            SystemMessage message ->
                                div [ class "system message" ]
                                    [ text message
                                    ]
                        )
                    )
                |> Html.Keyed.node "div" []
            , case chatInput of
                Just message ->
                    input
                        [ class "chatInput"
                        , value message
                        , onInput ChatInput
                        , id "chatInput"
                        ]
                        []

                Nothing ->
                    div [ class "chatHint" ] [ text "Press enter to chat" ]
            ]
        , uiButton Solid.users "Open player list" TogglePlayerList
        , uiButton Solid.map "Show map" ToggleMinimap
        ]


uiButton : Icon -> String -> msg -> Html msg
uiButton icon title msg =
    button
        [ class "uiButton"
        , onClick msg
        , Attributes.title title
        ]
        [ Icon.view [] icon ]


namePlates : Character -> Dict String ( Character, Vec2 ) -> Html GameMsg
namePlates player others =
    div [ class "namePlates overlay" ]
        [ Dict.toList others
            |> List.map
                (\( username, ( _, { x, y } ) ) ->
                    div
                        [ class "namePlate"
                        , style "top" (px ((player.coords.y - y) * 2))
                        , style "left" (px ((x - player.coords.x) * 2))
                        ]
                        [ text username ]
                )
            |> div []
        ]


playerList : Dict String ( Character, Vec2 ) -> Html GameMsg
playerList players =
    div [ class "overlay", onClick TogglePlayerList ]
        [ div [ class "modal" ]
            [ div [ class "header" ] [ textSpan "Players" ]
            , div [ class "body" ]
                (Dict.toList players
                    |> List.map
                        (\( username, ( character, coords ) ) ->
                            div [ class "player" ]
                                [ div [ class "avatar", style "background-image" ("url(" ++ Character.url character.skin ++ ")") ] []
                                , textSpan username
                                , textSpan ("x: " ++ String.fromInt (round character.coords.x))
                                , textSpan ("y: " ++ String.fromInt (round character.coords.y))
                                ]
                        )
                )
            ]
        ]


render : GameModel -> List Shape
render game =
    let
        terrain =
            game.chunks
                |> Dict.toList
                |> List.sortBy (Tuple.first >> Tuple.second)
                |> List.reverse
                |> List.filterMap
                    (\( ( x, y ), chunk ) ->
                        let
                            chunkVec =
                                { x = toFloat x, y = toFloat y }

                            diff =
                                Vec2.sub chunkVec (Vec2.scale (1 / World.chunkSize / Terrain.tileSize) game.player.coords)
                        in
                        Maybe.andThen
                            (\chunk_ ->
                                if abs diff.x < 1 && abs diff.y < 1 then
                                    World.render chunk_
                                        |> Playground.move
                                            (toFloat x * World.chunkSize * Terrain.tileSize)
                                            (toFloat y * World.chunkSize * Terrain.tileSize)
                                        |> Just

                                else
                                    Nothing
                            )
                            chunk
                    )
    in
    Playground.square Playground.black 2000
        :: [ terrain
                ++ (((game.player |> (\c -> ( c, c.coords ))) :: Dict.values game.others)
                        |> List.sortBy (Tuple.second >> .y >> negate)
                        |> List.map (\( c, coords ) -> Character.render game.time c |> Playground.move coords.x coords.y)
                   )
                |> Playground.group
                |> (if List.member Key.Spacebar game.keyboard then
                        Playground.move (negate game.player.coords.x) (negate game.player.coords.y)

                    else
                        Playground.scale 2
                            >> Playground.move (negate game.player.coords.x * 2) (negate game.player.coords.y * 2)
                   )
           , if game.showMinimap then
                Minimap.render game.chunks |> Playground.fade 0.8

             else
                Playground.square Playground.black 0
           ]


toScreen : Float -> Float -> Screen
toScreen width height =
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    }


requestTexture : Set String -> TextureManager -> ( TextureManager, Cmd GameMsg )
requestTexture missing textures =
    missing
        |> Set.foldl
            (\url (( { done, loading }, acc2 ) as acc) ->
                if Set.member url loading then
                    acc

                else
                    ( { loading = Set.insert url loading, done = done }
                    , (Texture.loadWith textureOption url
                        |> Task.map (Tuple.pair url)
                        |> Task.mapError (Tuple.pair url)
                        |> Task.attempt GotTexture
                      )
                        :: acc2
                    )
            )
            ( textures, [] )
        |> Tuple.mapSecond Cmd.batch


textureOption : Texture.Options
textureOption =
    { magnify = Texture.linear
    , minify = Texture.linear
    , horizontalWrap = Texture.clampToEdge
    , verticalWrap = Texture.clampToEdge
    , flipY = True
    }


gotTextures : Result ( String, Texture.Error ) ( String, Texture ) -> TextureManager -> TextureManager
gotTextures r ({ done, loading } as textures) =
    case r of
        Ok ( name, t ) ->
            { done =
                Dict.insert name t done
            , loading = Set.remove name loading
            }

        Err ( name, err ) ->
            textures
