module Page.Login exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (Html, button, div, h2, hr, input, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Json.Encode exposing (encode, object, string)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , formPassword : String
    , formEmail : String
    , loginFailed : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = "Login"
      , pageBody = "Log into your account."
      , formPassword = ""
      , formEmail = ""
      , loginFailed = False
      }
    , if Session.isLoggedIn session then
        Route.replaceUrl session.key Route.Landing

      else
        Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = model.pageTitle
    , content =
        div [ class "container" ]
            [ h2 [] [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            , hr [] []
            , viewForm model
            ]
    }


viewForm : Model -> Html Msg
viewForm model =
    div [ class "container" ]
        [ viewInput "text" "Email" model.formEmail CEmail
        , viewInput "password" "Password" model.formPassword CPassword
        , hr [] []
        , viewError model
        , hr [] []
        , button [ onClick Login ] [ text "Login" ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewError : Model -> Html msg
viewError model =
    if model.loginFailed then
        div [ style "color" "red" ] [ text "Login failed." ]

    else
        div [] []



-- UPDATE


type Msg
    = CEmail String
    | CPassword String
    | Login
    | LoginDone (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CEmail newEmail ->
            ( { model | formEmail = newEmail }, Cmd.none )

        CPassword newPassword ->
            ( { model | formPassword = newPassword }, Cmd.none )

        Login ->
            let
                newModel =
                    { model | loginFailed = False }
            in
            let
                theBody =
                    object
                        [ ( "logEmail", string newModel.formEmail )
                        , ( "logPassword", string newModel.formPassword )
                        ]
            in
            ( newModel
            , Http.post
                { url = "http://localhost:3000/auth/login"
                , body = Http.jsonBody theBody
                , expect = Http.expectJson LoginDone tokenDecoder
                }
            )

        LoginDone (Ok token) ->
            let
                newSession =
                    Session.logIn model.session token
            in
            ( { model | session = newSession }
            , Cmd.batch
                [ Route.replaceUrl newSession.key Route.Landing
                , Session.saveSession newSession
                ]
            )

        LoginDone (Err _) ->
            ( { model | loginFailed = True }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


tokenDecoder : Json.Decode.Decoder String
tokenDecoder =
    Json.Decode.field "authToken" Json.Decode.string
