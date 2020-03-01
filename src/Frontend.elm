module Frontend exposing (Model, app)

import Browser.Dom as Dom
import Browser.Events as Events
import Debug exposing (toString)
import Html exposing (Html, input, text)
import Html.Attributes exposing (autofocus, id, placeholder, style, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as D
import Lamdera
import Task
import Types exposing (..)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera
import Game.TwoD.Render as Render
import Game.Resources as Resources
import Color
import Keyboard
import Keyboard.Arrows as Arrows
import Simplex
import World


{-| Lamdera applications define 'app' instead of 'main'.

Lamdera.frontend is the same as Browser.application with the
additional update function; updateFromBackend.

-}
app =
    Lamdera.frontend
        { init = \_ _ -> init
        , update = update
        , updateFromBackend = updateFromBackend
        , view =
            \model ->
                { title = "Game"
                , body = [ view model ]
                }
        , subscriptions = subscriptions
        , onUrlChange = \_ -> Noop
        , onUrlRequest = \_ -> Noop
        }


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch
    [ Events.onResize ScreenSize
    , Sub.map Keys Keyboard.subscriptions
    , Events.onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)
    ]

type alias Model =
    FrontendModel


init : ( Model, Cmd FrontendMsg )
init =
    -- When the app loads, we have no messages and our message field is blank.
    -- We send an initial message to the backend, letting it know we've joined,
    -- so it knows to send us history and new messages
    (
        { messages = []
        , messageFieldContent = ""
        , character = {coords=(0,0)}
        , keys = []
        , time = 0
        , screen = (800,600)
        , resources = Resources.init
        }
        , Cmd.batch
            [ Lamdera.sendToBackend ClientJoin
            ,Task.perform (\{ viewport } -> ScreenSize (round viewport.width) (round viewport.height)) Dom.getViewport
        ]
    )



{-| This is the normal frontend update function. It handles all messages that can occur on the frontend.
-}
update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        ScreenSize width height ->
                    ( { model | screen = ( width, height ) }
                    , Cmd.none
                    )

        Tick dt ->
            (
             {
             model
              | character = updateCharacter dt model.keys model.character}
              {-{ model
                | mario = tick dt model.keys model.mario
                , time = dt + model.time
                , camera = Camera.moveTo ( model.mario.x, model.mario.y + 0.75 ) model.camera
              }-}
            , Cmd.none
            )
        Keys keyMsg ->
                    let
                        keys =
                            Keyboard.update keyMsg model.keys
                    in
                    ( { model | keys = keys }, Cmd.none )

        -- User has changed the contents of the message field
        MessageFieldChanged s ->
            ( { model | messageFieldContent = s }, Cmd.none )

        -- User has hit the Send button
        MessageSubmitted ->
            ( { model | messageFieldContent = "", messages = model.messages }
            , Cmd.batch
                [ Lamdera.sendToBackend (MsgSubmitted model.messageFieldContent)
                , focusOnMessageInputField
                , scrollChatToBottom
                ]
            )

        -- Empty msg that does no operations
        Noop ->
            ( model, Cmd.none )

updateCharacter dt keys character =
    let (x, y) = character.coords
        arrows = Arrows.wasd keys
    in
    {character | coords =
        (x + (toFloat arrows.x * dt * speed),y + (toFloat arrows.y * dt * speed))
    }

speed = 10


{-| This is the added update function. It handles all messages that can arrive from the backend.
-}
updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    ( case msg of
        ClientJoinReceived clientId ->
            { model | messages = ClientJoined clientId :: model.messages }

        RoomMsgReceived ( clientId, text ) ->
            { model | messages = MsgReceived clientId text :: model.messages }

        ClientTimeoutReceived clientId ->
            { model | messages = ClientTimedOut clientId :: model.messages }
    , Cmd.batch [ scrollChatToBottom ]
    )

{-
    Html.div (style "padding" "10px" :: fontStyles)
        [ model.messages
            |> List.reverse
            |> List.map viewMessage
            |> Html.div
                [ id "message-box"
                , style "height" "400px"
                , style "overflow" "auto"
                , style "margin-bottom" "15px"
                ]
        , chatInput model MessageFieldChanged
        , Html.button (onClick MessageSubmitted :: fontStyles) [ text "Send" ]

        ]
-}

view : Model -> Html FrontendMsg
view model =
    Html.div []
    [   Html.node "style" [] [text css]
    ,   Game.render
            { time = 0
            , size = model.screen
            , camera = Camera.fixedHeight 50 model.character.coords
            }
            ( Render.shape Render.rectangle {color=Color.yellow,position = model.character.coords,size = (1,1)}
             :: World.render)
    ]


css = """
body {
    background:black;
    margin:0;
}
"""


chatInput : Model -> (String -> FrontendMsg) -> Html FrontendMsg
chatInput model msg =
    input
        ([ id "message-input"
         , type_ "text"
         , onInput msg
         , onEnter MessageSubmitted
         , placeholder model.messageFieldContent
         , value model.messageFieldContent
         , style "width" "300px"
         , autofocus True
         ]
            ++ fontStyles
        )
        []


viewMessage : ChatMsg -> Html msg
viewMessage msg =
    case msg of
        ClientJoined clientId ->
            Html.div [ style "font-style" "italic" ] [ text <| clientId ++ " joined the chat" ]

        ClientTimedOut clientId ->
            Html.div [ style "font-style" "italic" ] [ text <| clientId ++ " left the chat" ]

        MsgReceived clientId message ->
            Html.div [] [ text <| "[" ++ clientId ++ "]: " ++ message ]


fontStyles : List (Html.Attribute msg)
fontStyles =
    [ style "font-family" "Helvetica", style "font-size" "14px", style "line-height" "1.5" ]


scrollChatToBottom : Cmd FrontendMsg
scrollChatToBottom =
    Dom.getViewportOf "message-box"
        |> Task.andThen (\info -> Dom.setViewportOf "message-box" 0 info.scene.height)
        |> Task.attempt (\_ -> Noop)


focusOnMessageInputField : Cmd FrontendMsg
focusOnMessageInputField =
    Task.attempt (always Noop) (Dom.focus "message-input")


onEnter : FrontendMsg -> Html.Attribute FrontendMsg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                D.succeed msg

            else
                D.fail "not ENTER"
    in
    on "keydown" (keyCode |> D.andThen isEnter)


timeoutInMs =
    5 * 1000
