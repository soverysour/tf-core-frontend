module Page.Landing exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Data.Paging exposing (..)
import Data.Transaction exposing (..)
import Html exposing (Html, button, div, h2, h5, hr, p, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Session exposing (Session)



-- MODEL


type ModelState
    = Loading
    | Failed
    | Success (Paginated Transaction)


type alias Model =
    { session : Session
    , pageTitle : String
    , pageBody : String
    , pageNumber : Int
    , state : ModelState
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , pageTitle = "Latest Transactions"
            , pageBody = "The latest submitted transactions."
            , pageNumber = 1
            , state = Loading
            }
    in
    ( model, getPage model )


getPage : Model -> Cmd Msg
getPage model =
    Http.get
        { url = "http://localhost:3000/offers?pageNumber=" ++ String.fromInt model.pageNumber
        , expect = Http.expectJson GotData (paginatedDecoder transactionDecoder)
        }



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = model.pageTitle
    , content =
        div [ class "container" ]
            [ h2 [] [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            , hr [] []
            , getRest model.state
            , div [ class "container" ]
                [ button [ onClick GoPrevPage ] [ text "Previous page" ]
                , text ("Currently on page " ++ String.fromInt model.pageNumber)
                , button [ onClick GoNextPage ] [ text "Next page" ]
                ]
            ]
    }


getRest : ModelState -> Html Msg
getRest state =
    case state of
        Loading ->
            h2 [] [ text "Loading..." ]

        Failed ->
            h2 [] [ text "Failed to retrieve the records. Please retry." ]

        Success transactionPage ->
            div [ class "container" ]
                [ h2 [] [ text "Here are the transactions." ]
                , ul [] (List.map transToHtml transactionPage.data)
                ]



-- UPDATE


type Msg
    = GotData (Result Http.Error (Paginated Transaction))
    | GoPrevPage
    | GoNextPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData (Err _) ->
            ( { model | state = Failed }, Cmd.none )

        GotData (Ok res) ->
            ( { model | state = Success res }, Cmd.none )

        GoPrevPage ->
            if model.pageNumber > 1 then
                let
                    newModel =
                        { model | pageNumber = model.pageNumber - 1 }
                in
                ( newModel, getPage newModel )

            else
                ( model, Cmd.none )

        GoNextPage ->
            case model.state of
                Success page ->
                    if page.count > model.pageNumber then
                        let
                            newModel =
                                { model | pageNumber = model.pageNumber + 1 }
                        in
                        ( newModel, getPage newModel )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
