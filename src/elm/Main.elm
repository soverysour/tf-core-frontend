module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode exposing (Value)
import Page
import Page.About as About
import Page.Blank as Blank
import Page.Landing as Landing
import Page.LinkAccount as LinkAccount
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Register as Register
import Page.SteamOpenIdReturn as SteamOpenIdReturn
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)



-- WARNING: Based on discussions around how asset management features
-- like code splitting and lazy loading have been shaping up, I expect
-- most of this file to become unnecessary in a future release of Elm.
-- Avoid putting things in here unless there is no alternative!
--
-- MODEL


type Model
    = Redirect Session
    | NotFound Session
    | Landing Landing.Model
    | About About.Model
    | Register Register.Model
    | Login Login.Model
    | LinkAccount LinkAccount.Model
    | SteamOpenIdReturn SteamOpenIdReturn.Model


init : Maybe String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init fields url navKey =
    case fields of
        Nothing ->
            changeRouteTo (Route.fromUrl url)
                (Redirect (Session.fromViewer navKey Nothing))
        Just str ->
            let
                maybeAuthToken =
                    case Json.Decode.decodeString Json.Decode.string str of
                        Err _ ->
                            Nothing

                        Ok token ->
                            Just token
            in
            changeRouteTo
                (Route.fromUrl url)
                (Redirect (Session.fromViewer navKey maybeAuthToken))



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            viewPage Page.Other (\_ -> Ignored) Blank.view

        NotFound _ ->
            viewPage Page.Other (\_ -> Ignored) NotFound.view

        Landing home ->
            viewPage Page.Landing GotLandingMsg (Landing.view home)

        About about ->
            viewPage Page.About GotAboutMsg (About.view about)

        Register register ->
            viewPage Page.Register GotRegisterMsg (Register.view register)

        Login login ->
            viewPage Page.Login GotLoginMsg (Login.view login)

        LinkAccount linkAccount ->
            viewPage Page.LinkAccount GotLinkAccountMsg (LinkAccount.view linkAccount)

        SteamOpenIdReturn steamOpenIdReturn ->
            viewPage Page.SteamOpenIdReturn GotSteamOpenIdReturnMsg (SteamOpenIdReturn.view steamOpenIdReturn)



-- UPDATE


type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotLandingMsg Landing.Msg
    | GotAboutMsg About.Msg
    | GotRegisterMsg Register.Msg
    | GotLoginMsg Login.Msg
    | GotLinkAccountMsg LinkAccount.Msg
    | GotSteamOpenIdReturnMsg SteamOpenIdReturn.Msg


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Landing home ->
            Landing.toSession home

        About about ->
            About.toSession about

        Register register ->
            Register.toSession register

        Login login ->
            Login.toSession login

        LinkAccount linkAccount ->
            LinkAccount.toSession linkAccount

        SteamOpenIdReturn linkAccount ->
            SteamOpenIdReturn.toSession linkAccount


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl session.key Route.Landing )

        Just Route.Landing ->
            Landing.init session
                |> updateWith Landing GotLandingMsg model

        Just Route.About ->
            About.init session
                |> updateWith About GotAboutMsg model

        Just Route.Register ->
            Register.init session
                |> updateWith Register GotRegisterMsg model

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg model

        Just Route.LinkAccount ->
            LinkAccount.init session
                |> updateWith LinkAccount GotLinkAccountMsg model

        Just (Route.SteamOpenIdReturn openIdData) ->
            SteamOpenIdReturn.init session openIdData
                |> updateWith SteamOpenIdReturn GotSteamOpenIdReturnMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (toSession model).key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotLandingMsg subMsg, Landing home ) ->
            Landing.update subMsg home
                |> updateWith Landing GotLandingMsg model

        ( GotAboutMsg subMsg, About about ) ->
            About.update subMsg about
                |> updateWith About GotAboutMsg model

        ( GotRegisterMsg subMsg, Register register ) ->
            Register.update subMsg register
                |> updateWith Register GotRegisterMsg model

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg model

        ( GotLinkAccountMsg subMsg, LinkAccount linkAccount ) ->
            LinkAccount.update subMsg linkAccount
                |> updateWith LinkAccount GotLinkAccountMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NotFound _ ->
            Sub.none

        Redirect _ ->
            Sub.none

        Landing home ->
            Sub.map GotLandingMsg (Landing.subscriptions home)

        About about ->
            Sub.map GotAboutMsg (About.subscriptions about)

        Register register ->
            Sub.map GotRegisterMsg (Register.subscriptions register)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        LinkAccount linkAccount ->
            Sub.map GotLinkAccountMsg (LinkAccount.subscriptions linkAccount)

        SteamOpenIdReturn steamOpenIdReturn ->
            Sub.map GotSteamOpenIdReturnMsg (SteamOpenIdReturn.subscriptions steamOpenIdReturn)



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
