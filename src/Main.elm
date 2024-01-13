module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (href)


main : Program () Model Msg
main =
    Browser.document
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
    ( Success, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotData ->
            ( Success, Cmd.none )


type Msg
    = GotData



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Frontend Quiz App"
    , body =
        [ viewHeader
        , viewMain model
        ]
    }


viewHeader : Html Msg
viewHeader =
    header [] []


viewMain : Model -> Html Msg
viewMain model =
    case model of
        Error ->
            main_ []
                [ p [] [ text "Error!" ] ]

        Loading ->
            main_ []
                [ p [] [ text "Loading..." ] ]

        Success ->
            main_ []
                [ header []
                    [ h1 []
                        [ span [] [ text "Welcome to the" ]
                        , span [] [ text "Frontend Quiz!" ]
                        ]
                    ]
                , p []
                    [ text "Pick a subject to get started." ]
                , ul []
                    [ li []
                        [ a [ href "/html" ] [ text "HTML" ] ]
                    , li []
                        [ a [ href "/css" ] [ text "CSS" ] ]
                    , li []
                        [ a [ href "/javascript" ] [ text "JavaScript" ] ]
                    , li []
                        [ a [ href "/accessibility" ] [ text "Accessibility" ] ]
                    ]
                ]
