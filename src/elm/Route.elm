module Route exposing (OpenIdData, Route(..), fromUrl, href, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Debug
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)
import Url.Parser.Query as Query



-- ROUTING --


type Route
    = Landing
    | Root
    | About
    | Register
    | Login
    | LinkAccount
    | SteamOpenIdReturn OpenIdData


type alias OpenIdData =
    { ns : Maybe String
    , mode : Maybe String
    , op_endpoint : Maybe String
    , claimed_id : Maybe String
    , identity : Maybe String
    , return_to : Maybe String
    , response_nonce : Maybe String
    , assoc_handle : Maybe String
    , signed : Maybe String
    , sig : Maybe String
    }


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Landing Parser.top
        , Parser.map About (s "about")
        , Parser.map Register (s "register")
        , Parser.map Login (s "login")
        , Parser.map LinkAccount (s "link-account")
        , Parser.map OpenIdData
            (s "link-account-steam"
                <?> Query.string "openid.ns"
                <?> Query.string "openid.mode"
                <?> Query.string "openid.op_endpoint"
                <?> Query.string "openid.claimed_id"
                <?> Query.string "openid.identity"
                <?> Query.string "openid.return_to"
                <?> Query.string "openid.response_nonce"
                <?> Query.string "openid.assoc_handle"
                <?> Query.string "openid.signed"
                <?> Query.string "openid.sig"
            )
            |> Parser.map SteamOpenIdReturn
        ]


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Landing ->
                    []

                Root ->
                    []

                About ->
                    [ "about" ]

                Register ->
                    [ "register" ]

                Login ->
                    [ "login" ]

                LinkAccount ->
                    [ "link-account" ]

                SteamOpenIdReturn _ ->
                    [ "link-account-steam" ]
    in
    "/" ++ String.join "/" pieces
