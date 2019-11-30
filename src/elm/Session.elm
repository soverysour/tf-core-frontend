port module Session exposing (Session, fromViewer, isLoggedIn, logIn, saveSession)

import Browser.Navigation as Nav
import Json.Encode


{-| Session management

This module is required to use Browser.application, which needs to store a Nav.Key
somewhere. This can evolve to have logged in Users

-}



-- TYPES


type alias Session =
    { key : Nav.Key
    , authToken : Maybe String
    }


fromViewer : Nav.Key -> Maybe String -> Session
fromViewer key maybeAuthToken =
    { key = key, authToken = maybeAuthToken }


logIn : Session -> String -> Session
logIn session token =
    { session | authToken = Just token }


isLoggedIn : Session -> Bool
isLoggedIn session =
    case session.authToken of
        Nothing ->
            False

        Just _ ->
            True


port storeToCache : Maybe Json.Encode.Value -> Cmd msg


saveSession : Session -> Cmd msg
saveSession session =
    Maybe.map Json.Encode.string session.authToken |> storeToCache
