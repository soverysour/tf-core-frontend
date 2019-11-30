module Page.SteamOpenIdReturn exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, button, div, h2, hr, input, text, ul)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Json.Encode exposing (Value, encode, object, string)
import Maybe exposing (withDefault)
import Request
import Route
import Session exposing (Session)
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , openIdData : Route.OpenIdData
    }


init : Session -> Route.OpenIdData -> ( Model, Cmd Msg )
init session openIdData =
    ( { session = session
      , pageTitle = "Linking Your Account"
      , pageBody = "Linking your account..."
      , openIdData = openIdData
      }
    , case session.authToken of
        Nothing ->
            Route.replaceUrl session.key Route.Landing

        Just token ->
            Request.postAuthEmpty token "http://localhost:3000/steam-openid" LinkingToSteam (toOpenIdObj openIdData)
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
            ]
    }


viewButton : String -> msg -> Html msg
viewButton message msg =
    button [ onClick msg ] [ text message ]



-- UPDATE


type Msg
    = LinkingToSteam (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkingToSteam (Ok _) ->
            ( model, Cmd.none )

        LinkingToSteam (Err _) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session


toOpenIdObj : Route.OpenIdData -> Value
toOpenIdObj openIdData =
    object
        [ ( "ns", string (withDefault "x" openIdData.ns) )
        , ( "mode", string (withDefault "x" openIdData.mode) )
        , ( "op_endpoint", string (withDefault "x" openIdData.op_endpoint) )
        , ( "claimed_id", string (withDefault "x" openIdData.claimed_id) )
        , ( "identity", string (withDefault "x" openIdData.identity) )
        , ( "return_to", string (withDefault "x" openIdData.return_to) )
        , ( "response_nonce", string (withDefault "x" openIdData.response_nonce) )
        , ( "assoc_handle", string (withDefault "x" openIdData.assoc_handle) )
        , ( "signed", string (withDefault "x" openIdData.signed) )
        , ( "sig", string (withDefault "x" openIdData.sig) )
        ]
