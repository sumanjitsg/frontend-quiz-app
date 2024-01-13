module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, src)


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
    header [ class "container page-header" ]
        [ nav [] []
        ]


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
            main_ [ class "container" ]
                [ header []
                    [ h1 []
                        [ span [] [ text "Welcome to the" ]
                        , text "Frontend Quiz!"
                        ]
                    , p []
                        [ text "Pick a subject to get started." ]
                    ]
                , ul []
                    [ li []
                        [ a [ href "/html" ] [ img [ src "/assets/images/icon-html.svg" ] [], text "HTML" ] ]
                    , li []
                        [ a [ href "/css" ] [ img [ src "/assets/images/icon-css.svg" ] [], text "CSS" ] ]
                    , li []
                        [ a [ href "/javascript" ] [ img [ src "/assets/images/icon-js.svg" ] [], text "JavaScript" ] ]
                    , li []
                        [ a [ href "/accessibility" ] [ img [ src "/assets/images/icon-accessibility.svg" ] [], text "Accessibility" ] ]
                    ]
                ]
