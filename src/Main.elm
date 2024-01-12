module Main exposing (main)

import Browser
import Html exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Error
    | Loading
    | Success


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        _ ->
            ( Success, Cmd.none )


type Msg
    = GotData



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Error ->
            div [] [ text "Error!" ]

        Loading ->
            div [] [ text "Loading..." ]

        Success ->
            div [] [ text "Success." ]
