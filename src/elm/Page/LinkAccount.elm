module Page.LinkAccount exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, button, div, h2, hr, input, text, ul)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Json.Encode exposing (encode, object, string)
import Request
import Route
import Session exposing (Session)
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , steamJanrain : Maybe String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , pageTitle = "Link Your Account"
      , pageBody = "Link your account with an external inventory provider."
      , steamJanrain = Nothing
      }
    , if not (Session.isLoggedIn session) then
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
            , ul []
                [ viewButton "Link Steam Account" RedirectToSteam
                ]
            ]
    }


viewButton : String -> msg -> Html msg
viewButton message msg =
    button [ onClick msg ] [ text message ]



-- UPDATE


type Msg
    = RedirectToSteam


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RedirectToSteam ->
            ( model, Nav.load steamRedirectToUrl )


steamRedirectToUrl : String
steamRedirectToUrl =
    UrlBuilder.crossOrigin "https://steamcommunity.com"
        [ "openid", "login" ]
        [ UrlBuilder.string "openid.ns" "http://specs.openid.net/auth/2.0"
        , UrlBuilder.string "openid.mode" "checkid_setup"
        , UrlBuilder.string "openid.claimed_id" "http://specs.openid.net/auth/2.0/identifier_select"
        , UrlBuilder.string "openid.identity" "http://specs.openid.net/auth/2.0/identifier_select"
        , UrlBuilder.string "openid.return_to" "http://localhost:8080/link-account-steam"
        , UrlBuilder.string "openid.realm" "http://localhost:8080/"
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
