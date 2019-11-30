module Data.Transaction exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, src)
import Json.Decode exposing (..)


transToHtml : Transaction -> Html msg
transToHtml transaction =
    let
        createdAt =
            transaction.details.createdAt

        author =
            transaction.authorDetails.nickname

        rank =
            String.fromFloat transaction.authorDetails.avgRank
    in
    div [ class "container" ]
        [ h4 [] [ text ("Created at " ++ createdAt ++ " by " ++ author ++ ". Rank: " ++ rank) ]
        , hr [] []
        , h3 [] [ text "Selling: " ]
        , div [ class "container" ] (List.map itemToHtml transaction.wtsItems)
        , h3 [] [ text "Expecting: " ]
        , div [ class "container" ] (List.map itemToHtml transaction.wtbItems)
        , hr [] []
        , hr [] []
        ]


type alias Transaction =
    { details : TransactionDetails
    , wtsItems : List Item
    , wtbItems : List Item
    , authorDetails : UserDetails
    }


transactionDecoder : Decoder Transaction
transactionDecoder =
    map4 Transaction
        (field "getTransaction" transactionDetailsDecoder)
        (field "getWtsItems" (list itemDecoder))
        (field "getWtbItems" (list itemDecoder))
        (field "getUserDetails" userDetailsDecoder)


type alias TransactionDetails =
    { createdAt : String
    , id : Int
    }


transactionDetailsDecoder : Decoder TransactionDetails
transactionDetailsDecoder =
    map2 TransactionDetails
        (field "createdAt" string)
        (field "transactionId" int)


type alias Item =
    { name : String
    , description : String
    , iconUrl : String
    , properties : List ItemProp
    }


itemToHtml : Item -> Html msg
itemToHtml item =
    div [ class "container" ]
        [ h3 [] [ text ("Item: " ++ item.name) ]
        , img [ src item.iconUrl ] []
        , p [] [ text item.description ]
        , ul [] (List.map propToHtml item.properties)
        ]


itemDecoder : Decoder Item
itemDecoder =
    map4 Item
        (field "itemName" string)
        (field "itemDescription" string)
        (field "itemIconUrl" string)
        (field "itemProps" (list itemPropDecoder))


type alias ItemProp =
    { name : String
    , value : String
    }


propToHtml : ItemProp -> Html msg
propToHtml prop =
    text (prop.name ++ " -> " ++ prop.value)


itemPropDecoder : Decoder ItemProp
itemPropDecoder =
    map2 ItemProp
        (field "itemPropName" string)
        (field "itemPropValue" string)


type alias UserDetails =
    { id : Int
    , nickname : String
    , avgRank : Float
    }


userDetailsDecoder : Decoder UserDetails
userDetailsDecoder =
    map3 UserDetails
        (field "getUserId" int)
        (field "getNickname" string)
        (field "getAvgRank" float)
