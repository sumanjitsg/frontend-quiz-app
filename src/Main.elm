module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Http
import Json.Decode as Json
import Url exposing (Url)
import Url.Parser as Url


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
    , data : DataState
    }


type DataState
    = Loading
    | Error
    | Success Quizzes


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model (toRoute url) key Loading, getQuizzes )



-- ROUTE


type Route
    = HomePage
    | TopicPage String



-- toGameStateFromString : String -> Maybe GameState
-- toGameStateFromString topicName =
--     case toTopic topicName of
--         Just topic ->
--             toGameState topic quizzes
--         _ ->
--             Nothing


toGameState : Topic -> Quizzes -> Maybe GameState
toGameState topic quizzes =
    let
        maybeQuestions =
            Dict.get topic.displayName quizzes
    in
    case maybeQuestions of
        Just questions ->
            Just
                { questions = questions
                , currentScore = 0
                , currentQuestion = 1
                }

        Nothing ->
            Nothing


type alias GameState =
    { questions : List Question
    , currentScore : Int
    , currentQuestion : Int
    }


routeParser : Url.Parser (Route -> a) a
routeParser =
    Url.oneOf
        [ Url.map HomePage Url.top
        , Url.map TopicPage Url.string
        ]


toRoute : Url -> Route
toRoute url =
    case Url.parse routeParser url of
        Just route ->
            route

        Nothing ->
            HomePage



-- UPDATE


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | ReceivedQuizzes (Result Http.Error Quizzes)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key <|
                        Url.toString url
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        UrlChanged url ->
            ( { model | route = toRoute url }
            , Cmd.none
            )

        ReceivedQuizzes result ->
            case result of
                Ok quizzes ->
                    ( { model | data = Success quizzes }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | data = Error }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Frontend Quiz App"
    , body = [ viewHeader model.route, viewMain model ]
    }


viewHeader : Route -> Html Msg
viewHeader route =
    header
        [ class "container header" ]
        [ nav []
            [ case route of
                HomePage ->
                    text ""

                TopicPage topicName ->
                    case toTopic topicName of
                        Just topic ->
                            div
                                [ class "header__image-text text--medium" ]
                                [ img
                                    [ src topic.logoSrc ]
                                    []
                                , span []
                                    [ text topic.displayName ]
                                ]

                        Nothing ->
                            text ""
            ]
        ]


viewMain : Model -> Html Msg
viewMain model =
    main_
        [ class "container" ]
    <|
        case model.route of
            HomePage ->
                [ header
                    [ class "header" ]
                    [ h1 [ class "text--xl" ]
                        [ div []
                            [ text "Welcome to the" ]
                        , div []
                            [ text "Frontend Quiz!" ]
                        ]
                    , p [ class "text--italic" ]
                        [ text "Pick a subject to get started." ]
                    ]

                -- List of quiz topics
                , ul
                    [ class "list text--medium" ]
                  <|
                    List.map
                        (\topic ->
                            li [ class "list-item" ]
                                [ a
                                    [ href topic.urlPath ]
                                    [ img
                                        [ src topic.logoSrc ]
                                        []
                                    , span []
                                        [ text topic.displayName ]
                                    ]
                                ]
                        )
                        topics
                ]

            TopicPage topicName ->
                let
                    maybeGameState =
                        case model.data of
                            Success quizzes ->
                                case toTopic topicName of
                                    Just topic ->
                                        toGameState topic quizzes

                                    Nothing ->
                                        Nothing

                            _ ->
                                Nothing
                in
                case maybeGameState of
                    Just gameState ->
                        let
                            _ =
                                Debug.log "game state" gameState
                        in
                        [ div []
                            [ p [ class "text--italic" ]
                                [ text ("Question " ++ String.fromInt gameState.currentQuestion ++ " of 10") ]
                            , p [ class "text--large" ]
                                [ case List.head gameState.questions of
                                    Just question ->
                                        text question.title

                                    Nothing ->
                                        text ""
                                ]
                            ]
                        , div [ class "list text--medium" ]
                            [ case List.head gameState.questions of
                                Just question ->
                                    ul [ class "list" ] <|
                                        List.map
                                            (\option ->
                                                li [ class "list-item" ]
                                                    [ div [] [ text option ]
                                                    ]
                                            )
                                            question.options

                                Nothing ->
                                    text ""
                            ]
                        ]

                    Nothing ->
                        []



-- HTTP


getQuizzes =
    Http.get
        { url = "/data.json"
        , expect = Http.expectJson ReceivedQuizzes responseDecoder
        }



-- DECODERS


type alias Quizzes =
    Dict String (List Question)


type alias Question =
    { title : String
    , options : List String
    , answer : String
    }


responseDecoder : Json.Decoder Quizzes
responseDecoder =
    Json.field "quizzes" quizzesDecoder


quizzesDecoder : Json.Decoder Quizzes
quizzesDecoder =
    Json.map Dict.fromList <|
        Json.list quizDecoder


quizDecoder : Json.Decoder ( String, List Question )
quizDecoder =
    Json.map2
        (\topicName questions ->
            ( topicName, questions )
        )
        (Json.field "title" Json.string)
        (Json.field "questions" <|
            Json.list questionDecoder
        )


questionDecoder : Json.Decoder Question
questionDecoder =
    Json.map3
        Question
        (Json.field "question" Json.string)
        (Json.field "options" <|
            Json.list Json.string
        )
        (Json.field "answer" Json.string)


type alias Topic =
    { urlPath : String
    , logoSrc : String
    , displayName : String
    }


toTopic : String -> Maybe Topic
toTopic topicName =
    let
        urlPath =
            "/" ++ String.toLower topicName
    in
    topics
        |> List.filter (\topic -> topic.urlPath == urlPath)
        |> List.head


topics : List Topic
topics =
    [ { urlPath = "/html"
      , logoSrc = "/assets/images/icon-html.svg"
      , displayName = "HTML"
      }
    , { urlPath = "/css"
      , logoSrc = "/assets/images/icon-css.svg"
      , displayName = "CSS"
      }
    , { urlPath = "/javascript"
      , logoSrc = "/assets/images/icon-js.svg"
      , displayName = "JavaScript"
      }
    , { urlPath = "/accessibility"
      , logoSrc = "/assets/images/icon-accessibility.svg"
      , displayName = "Accessibility"
      }
    ]
