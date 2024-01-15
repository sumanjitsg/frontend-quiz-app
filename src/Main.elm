module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Url
import Url.Parser as Parser exposing (Parser)
import VitePluginHelper exposing (asset)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { route : Route
    , key : Navigation.Key
    }


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model (toRoute url) key, Cmd.none )



-- ROUTE


type Route
    = HomePage
    | TopicPage String


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomePage Parser.top
        , Parser.map TopicPage Parser.string
        ]


toRoute : Url.Url -> Route
toRoute url =
    case Parser.parse routeParser url of
        Just route ->
            route

        Nothing ->
            HomePage



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key <| Url.toString url
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        UrlChanged url ->
            ( { model | route = toRoute url }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view _ =
    { title = "Frontend Quiz App"
    , body = [ viewHeader, viewMain ]
    }


viewHeader : Html Msg
viewHeader =
    header
        [ class "container body__header" ]
        [ nav [] [] ]


viewMain : Html Msg
viewMain =
    main_
        [ class "container" ]
        [ header
            [ class "main__header" ]
            [ h1 [ class "text--heading" ]
                [ div []
                    [ text "Welcome to the" ]
                , div []
                    [ text "Frontend Quiz!" ]
                ]
            , p [ class "text--italic" ]
                [ text "Pick a subject to get started." ]
            ]
        , ul
            [ class "list text--medium" ]
            [ li []
                [ a
                    [ href "/html"
                    , class "list__item"
                    ]
                    [ img
                        [ src <| asset "/assets/images/icon-html.svg" ]
                        []
                    , span []
                        [ text "HTML" ]
                    ]
                ]
            , li []
                [ a
                    [ href "/css"
                    , class "list__item"
                    ]
                    [ img
                        [ src <| asset "/assets/images/icon-css.svg" ]
                        []
                    , span []
                        [ text "CSS" ]
                    ]
                ]
            , li []
                [ a
                    [ href "/javascript"
                    , class "list__item"
                    ]
                    [ img
                        [ src <| asset "/assets/images/icon-js.svg" ]
                        []
                    , span []
                        [ text "JavaScript" ]
                    ]
                ]
            , li []
                [ a
                    [ href "/accessibility"
                    , class "list__item"
                    ]
                    [ img
                        [ src <| asset "/assets/images/icon-accessibility.svg" ]
                        []
                    , span []
                        [ text "Accessibility" ]
                    ]
                ]
            ]
        ]
