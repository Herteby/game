module Frontend exposing (Model, app)

import Browser.Dom as Dom
import Browser.Events as Events
import Debug exposing (toString)
import Html exposing (Html, input, text)
import Html.Attributes exposing (src,autofocus, id, placeholder, style, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as D
import Lamdera
import Task
import Types exposing (..)
import Color
import Keyboard
import Keyboard.Arrows as Arrows
import Simplex
import World
import Playground exposing (Computer, Shape)
import Playground.Advanced as Playground
import Playground.Extra as Playground


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
    Sub.map GameMsg Playground.subscriptions.all


type alias Model =
    FrontendModel


init : ( Model, Cmd FrontendMsg )
init =
    -- When the app loads, we have no messages and our message field is blank.
    -- We send an initial message to the backend, letting it know we've joined,
    -- so it knows to send us history and new messages
    let (gameModel, gameCmd) = game.init
    in
    ({ messages = []
                         , messageFieldContent = ""
                         , game = gameModel
                         }

        , Cmd.batch
            [Cmd.map GameMsg gameCmd,
            Lamdera.sendToBackend ClientJoin
              ]
    )


{-| This is the normal frontend update function. It handles all messages that can occur on the frontend.
-}
update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of


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
        GameMsg submsg ->
            game.update submsg model.game
            |> Tuple.mapBoth (\g -> {model | game = g}) (Cmd.map GameMsg)

        -- Empty msg that does no operations
        Noop ->
            ( model, Cmd.none )




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
     , game.view model.game
    ]


game  =
    Playground.embed render updateGame
    {
        character = {coords = (0,0)}
    }


render : Computer -> Memory -> List Shape
render computer memory =
    World.render ++
    [Playground.circle Playground.red 10
    |> Playground.move (Tuple.first memory.character.coords) (Tuple.second memory.character.coords)
    ]

updateGame : Computer -> Memory -> Memory
updateGame computer memory =
    {memory | character = updateCharacter computer memory.character}


updateCharacter computer character =
    let (x, y) = character.coords
        d = Playground.delta computer.time |> toFloat
    in
    {character | coords =
        (x + (Playground.toX computer.keyboard * speed * d)
        ,y + (Playground.toY computer.keyboard * speed * d))
    }


updateCharacter2 dt keys character =
    let (x, y) = character.coords
        arrows = Arrows.wasd keys
    in
    {character | coords =
        (x + (toFloat arrows.x * dt * speed),y + (toFloat arrows.y * dt * speed))
    }

speed = 0.5

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
