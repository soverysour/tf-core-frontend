module Request exposing (..)

import Http
import Json.Decode
import Json.Encode exposing (..)


getAuth : String -> String -> (Result Http.Error b -> msg) -> Json.Decode.Decoder b -> Cmd msg
getAuth token url toMsg decoder =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


postAuth : String -> String -> (Result Http.Error b -> msg) -> Value -> Json.Decode.Decoder b -> Cmd msg
postAuth token url toMsg body decoder =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


postAuthEmpty : String -> String -> (Result Http.Error () -> msg) -> Value -> Cmd msg
postAuthEmpty token url toMsg body =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectWhatever toMsg
        , timeout = Nothing
        , tracker = Nothing
        }
