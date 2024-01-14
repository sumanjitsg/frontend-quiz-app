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
        [ class "container page-header" ]
        [ nav [] [] ]


viewMain : Html Msg
viewMain =
    main_
        [ class "container" ]
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
                [ a [ href "/html" ]
                    [ img [ src <| asset "/assets/images/icon-html.svg" ] []
                    , text "HTML"
                    ]
                ]
            , li []
                [ a [ href "/css" ]
                    [ img [ src <| asset "/assets/images/icon-css.svg" ] []
                    , text "CSS"
                    ]
                ]
            , li []
                [ a [ href "/javascript" ]
                    [ img [ src <| asset "/assets/images/icon-js.svg" ] []
                    , text "JavaScript"
                    ]
                ]
            , li []
                [ a [ href "/accessibility" ]
                    [ img [ src <| asset "/assets/images/icon-accessibility.svg" ] []
                    , text "Accessibility"
                    ]
                ]
            ]
        ]
