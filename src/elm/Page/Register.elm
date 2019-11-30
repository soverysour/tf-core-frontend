module Page.Register exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (Html, button, div, h2, hr, input, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode exposing (encode, object, string)
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , formPassword : String
    , formPasswordVerify : String
    , formNickname : String
    , formEmail : String
    , registrationFailed : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = "Register"
      , pageBody = "Create a new account below."
      , formPassword = ""
      , formPasswordVerify = ""
      , formNickname = ""
      , formEmail = ""
      , registrationFailed = False
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
        [ viewInput "text" "Nickname" model.formNickname CNickname
        , viewInput "text" "Email" model.formEmail CEmail
        , viewInput "password" "Password" model.formPassword CPassword
        , viewInput "password" "Re-enter Password" model.formPasswordVerify CPasswordVerify
        , hr [] []
        , viewValidation model
        , viewError model
        , hr [] []
        , button [ onClick Register ] [ text "Register" ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewError : Model -> Html msg
viewError model =
    if model.registrationFailed then
        div [ style "color" "red" ] [ text "Registration Failed" ]

    else
        div [] []


viewValidation : Model -> Html msg
viewValidation model =
    if model.formPassword == model.formPasswordVerify then
        div [ style "color" "green" ] [ text "OK" ]

    else
        div [ style "color" "red" ] [ text "Passwords do not match!" ]



-- UPDATE


type Msg
    = CEmail String
    | CNickname String
    | CPassword String
    | CPasswordVerify String
    | Register
    | RegistrationDone (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CEmail newEmail ->
            ( { model | formEmail = newEmail }, Cmd.none )

        CNickname newNickname ->
            ( { model | formNickname = newNickname }, Cmd.none )

        CPassword newPassword ->
            ( { model | formPassword = newPassword }, Cmd.none )

        CPasswordVerify newPasswordVerify ->
            ( { model | formPasswordVerify = newPasswordVerify }, Cmd.none )

        Register ->
            let
                newModel =
                    { model | registrationFailed = False }
            in
            if newModel.formPassword /= newModel.formPasswordVerify then
                ( newModel, Cmd.none )

            else
                let
                    theBody =
                        object
                            [ ( "regEmail", string newModel.formEmail )
                            , ( "regNickname", string newModel.formNickname )
                            , ( "regPassword", string newModel.formPassword )
                            ]
                in
                ( newModel
                , Http.post
                    { url = "http://localhost:3000/auth/register"
                    , body = Http.jsonBody theBody
                    , expect = Http.expectWhatever RegistrationDone
                    }
                )

        RegistrationDone (Ok _) ->
            ( model, Route.replaceUrl model.session.key Route.Login )

        RegistrationDone (Err _) ->
            ( { model | registrationFailed = True }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
