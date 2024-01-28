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



-- MAIN


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



-- INIT


type alias Model =
    { route : Route
    , key : Navigation.Key
    , quiz : Quiz
    }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model (toRoute url) key initialQuiz, getQuizData )



-- ROUTE


type Route
    = HomePage
    | TopicPage Topic


routeParser : Url.Parser (Route -> a) a
routeParser =
    Url.oneOf
        [ Url.map HomePage Url.top
        , Url.map TopicPage topicParser
        ]


topicParser : Url.Parser (Topic -> a) a
topicParser =
    Url.oneOf
        [ Url.map Html (Url.s "html")
        , Url.map Css (Url.s "css")
        , Url.map JavaScript (Url.s "javascript")
        , Url.map Accessibility (Url.s "accessibility")
        ]


toRoute : Url -> Route
toRoute url =
    case Url.parse routeParser url of
        Just route ->
            route

        Nothing ->
            -- TODO: redirect to HomePage
            HomePage



-- UPDATE


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | ReceivedQuizData (Result Http.Error JsonQuizVault)


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

        ReceivedQuizData data ->
            case data of
                Ok jsonQuizVault ->
                    let
                        updatedVault =
                            updateQuizVault jsonQuizVault model.quiz.vault

                        quiz =
                            model.quiz

                        updatedQuiz =
                            { quiz | vault = updatedVault }
                    in
                    ( { model | quiz = updatedQuiz }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )



-- case data of
--     Ok jsonQuizVault ->
--         ( { model | topics = updateQuizVault jsonQuizVault model.topics }
--         , Cmd.none
--         )
--     Err _ ->
--         ( model, Cmd.none )
-- TODO: what happens if a route is opened directly?


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
                    Debug.todo "topic page"

            -- case toTopic topicName of
            --     Just topic ->
            --         div
            --             [ class "header__image-text text--medium" ]
            --             [ img
            --                 [ src topic.logoSrc ]
            --                 []
            --             , span []
            --                 [ text topic.displayName ]
            --             ]
            --     Nothing ->
            --         text ""
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
                    (List.map
                        (\topic ->
                            let
                                topicName =
                                    toTopicString topic
                            in
                            case Dict.get topicName model.quiz.metadata of
                                Just topicInfo ->
                                    li
                                        [ class "list-item" ]
                                        [ case Dict.get topicName model.quiz.vault of
                                            Just topicQuestions ->
                                                if topicQuestions.current == Nothing then
                                                    a
                                                        []
                                                        [ img
                                                            [ src topicInfo.logoSrc ]
                                                            []
                                                        , span []
                                                            [ text topicInfo.displayName ]
                                                        ]

                                                else
                                                    a
                                                        [ href topicInfo.urlPath ]
                                                        [ img
                                                            [ src topicInfo.logoSrc ]
                                                            []
                                                        , span []
                                                            [ text topicInfo.displayName ]
                                                        ]

                                            Nothing ->
                                                text ""
                                        ]

                                Nothing ->
                                    -- TODO: handle this case
                                    text ""
                        )
                        model.quiz.topics
                    )
                ]

            TopicPage topicName ->
                Debug.todo "topic page"



-- let
--     maybeGameState =
--         case model.data of
--             Success quizzes ->
--                 case toTopic topicName of
--                     Just topic ->
--                         toGameState topic quizzes
--                     Nothing ->
--                         Nothing
--             _ ->
--                 Nothing
-- in
-- case maybeGameState of
--     Just gameState ->
--         -- let
--         --     _ =
--         --         Debug.log "game state" gameState
--         -- in
--         [ div []
--             [ p [ class "text--italic" ]
--                 [ text ("Question " ++ String.fromInt gameState.currentQuestion ++ " of 10") ]
--             , p [ class "text--large" ]
--                 [ case List.head gameState.questions of
--                     Just question ->
--                         text question.title
--                     Nothing ->
--                         text ""
--                 ]
--             ]
--         , div [ class "list text--medium" ]
--             [ case List.head gameState.questions of
--                 Just question ->
--                     ul [ class "list" ] <|
--                         List.map
--                             (\option ->
--                                 li [ class "list-item" ]
--                                     [ div [] [ text option ]
--                                     ]
--                             )
--                             question.options
--                 Nothing ->
--                     text ""
--             ]
--         ]
--     Nothing ->
--         []
-- HTTP


getQuizData =
    Http.get
        { url = "/data.json"
        , expect = Http.expectJson ReceivedQuizData responseDecoder
        }



-- DECODERS


responseDecoder : Json.Decoder JsonQuizVault
responseDecoder =
    Json.field "quizzes" quizVaultDecoder


quizVaultDecoder : Json.Decoder JsonQuizVault
quizVaultDecoder =
    Json.map Dict.fromList <| Json.list topicQuestionsDecoder


topicQuestionsDecoder : Json.Decoder ( String, JsonTopicQuestions )
topicQuestionsDecoder =
    Json.map2
        Tuple.pair
        (Json.field "title" Json.string)
        (Json.field "questions" <| Json.list questionDecoder)


questionDecoder : Json.Decoder JsonQuestion
questionDecoder =
    Json.map3
        JsonQuestion
        (Json.field "question" Json.string)
        (Json.field "options" <|
            Json.list Json.string
        )
        (Json.field "answer" Json.string)



-- QUIZ


type alias QuizVault =
    Dict String TopicQuestions


type alias JsonQuizVault =
    Dict String JsonTopicQuestions



-- toTopicQuestions : Topic -> JsonQuizVault -> TopicQuestions
-- toTopicQuestions topic quizzes =
--     Dict.get (toTopicString topic) quizzes
--         |> Maybe.map toQuestions
--         |> Maybe.withDefault initialTopicQuestions


updateQuizVault : JsonQuizVault -> QuizVault -> QuizVault
updateQuizVault jsonQuizVault quizVault =
    Dict.foldl addTopicQuestions quizVault jsonQuizVault


addTopicQuestions : String -> JsonTopicQuestions -> QuizVault -> QuizVault
addTopicQuestions jsonTopicName jsonTopicQuestions quizVault =
    case toTopic (String.toLower jsonTopicName) of
        Just topic ->
            let
                topicName =
                    toTopicString topic
            in
            case Dict.get topicName quizVault of
                Just topicQuestions ->
                    Dict.insert
                        topicName
                        (updateTopicQuestions
                            jsonTopicQuestions
                            topicQuestions
                        )
                        quizVault

                Nothing ->
                    quizVault

        Nothing ->
            quizVault


type alias JsonTopicQuestions =
    List JsonQuestion


type alias TopicQuestions =
    { current : Maybe Question
    , next : List Question
    }


initialTopicQuestions : TopicQuestions
initialTopicQuestions =
    { current = Nothing
    , next = []
    }


updateTopicQuestions : JsonTopicQuestions -> TopicQuestions -> TopicQuestions
updateTopicQuestions jsonQuestions questions =
    List.foldl addQuestion questions jsonQuestions


addQuestion : JsonQuestion -> TopicQuestions -> TopicQuestions
addQuestion jsonQuestion questions =
    case toQuestion jsonQuestion of
        Just question ->
            if questions.current == Nothing then
                { questions | current = Just question }

            else
                { questions | next = questions.next ++ [ question ] }

        Nothing ->
            questions


type alias Question =
    { question : String
    , options : Options
    }


type alias JsonQuestion =
    { question : String
    , options : List String
    , answer : String
    }


toQuestion : JsonQuestion -> Maybe Question
toQuestion jsonQuestion =
    toOptions jsonQuestion.answer jsonQuestion.options
        |> Maybe.map (\options -> { question = jsonQuestion.question, options = options })


type alias Options =
    { a : ( String, Bool )
    , b : ( String, Bool )
    , c : ( String, Bool )
    , d : ( String, Bool )
    }


toOptions : String -> List String -> Maybe Options
toOptions answer options =
    -- TODO: refactor with Maybe.andThen or Maybe.map?
    case options of
        [ a, b, c, d ] ->
            if List.member answer options then
                Just
                    { a = ( a, a == answer )
                    , b = ( b, b == answer )
                    , c = ( c, c == answer )
                    , d = ( d, d == answer )
                    }

            else
                Nothing

        _ ->
            Nothing


type alias Quiz =
    { topics : List Topic
    , vault : QuizVault
    , metadata : Dict String TopicMetadata
    }


type alias TopicMetadata =
    { urlPath : String
    , logoSrc : String
    , displayName : String
    }


type Topic
    = Html
    | Css
    | JavaScript
    | Accessibility


initialQuiz : Quiz
initialQuiz =
    { topics = [ Html, Css, JavaScript, Accessibility ]
    , vault =
        Dict.fromList
            [ ( toTopicString Html, initialTopicQuestions )
            , ( toTopicString Css, initialTopicQuestions )
            , ( toTopicString JavaScript, initialTopicQuestions )
            , ( toTopicString Accessibility, initialTopicQuestions )
            ]
    , metadata =
        Dict.fromList
            [ ( toTopicString Html
              , { urlPath = "/html"
                , logoSrc = "/assets/images/icon-html.svg"
                , displayName = "HTML"
                }
              )
            , ( toTopicString Css
              , { urlPath = "/css"
                , logoSrc = "/assets/images/icon-css.svg"
                , displayName = "CSS"
                }
              )
            , ( toTopicString JavaScript
              , { urlPath = "/javascript"
                , logoSrc = "/assets/images/icon-js.svg"
                , displayName = "JavaScript"
                }
              )
            , ( toTopicString Accessibility
              , { urlPath = "/accessibility"
                , logoSrc = "/assets/images/icon-accessibility.svg"
                , displayName = "Accessibility"
                }
              )
            ]
    }


toTopicString : Topic -> String
toTopicString topic =
    case topic of
        Html ->
            "html"

        Css ->
            "css"

        JavaScript ->
            "javascript"

        Accessibility ->
            "accessibility"


toTopic : String -> Maybe Topic
toTopic topicName =
    case topicName of
        "html" ->
            Just Html

        "css" ->
            Just Css

        "javascript" ->
            Just JavaScript

        "accessibility" ->
            Just Accessibility

        _ ->
            Nothing



{- TODO: split urlPath and check if last one is equal to topicName -}
