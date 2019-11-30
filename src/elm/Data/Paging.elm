module Data.Paging exposing (..)

import Json.Decode exposing (..)


type alias Paginated a =
    { count : Int
    , data : List a
    }


paginatedDecoder : Decoder a -> Decoder (Paginated a)
paginatedDecoder dec =
    map2 Paginated
        (field "pageCount" int)
        (field "pageData" (list dec))
