module Quiz exposing (Options, Question, Quiz, QuizVault, Topic(..), TopicMetadata, TopicQuestions, addQuestion, addTopicQuestions, toOptions, toQuestion, toTopic, toTopicString, updateQuizVault, updateTopicQuestions)

import Api
import Dict exposing (Dict)


type alias Quiz =
    { topics : List Topic
    , vault : QuizVault
    , metadata : Dict String TopicMetadata
    }


type alias QuizVault =
    Dict String TopicQuestions


updateQuizVault : Api.JsonQuizVault -> QuizVault -> QuizVault
updateQuizVault jsonQuizVault quizVault =
    Dict.foldl addTopicQuestions quizVault jsonQuizVault


type alias TopicQuestions =
    { current : Maybe Question
    , next : List Question
    , currentIndex : Maybe Int
    }


addTopicQuestions : String -> Api.JsonTopicQuestions -> QuizVault -> QuizVault
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


updateTopicQuestions : Api.JsonTopicQuestions -> TopicQuestions -> TopicQuestions
updateTopicQuestions jsonQuestions questions =
    List.foldl addQuestion questions jsonQuestions


addQuestion : Api.JsonQuestion -> TopicQuestions -> TopicQuestions
addQuestion jsonQuestion questions =
    case toQuestion jsonQuestion of
        Just question ->
            if questions.current == Nothing then
                { questions
                    | current = Just question
                    , currentIndex = Just 1
                }

            else
                { questions | next = questions.next ++ [ question ] }

        Nothing ->
            questions


type alias Question =
    { question : String
    , options : Options
    }


toQuestion : Api.JsonQuestion -> Maybe Question
toQuestion jsonQuestion =
    toOptions jsonQuestion.answer jsonQuestion.options
        |> Maybe.map (\options -> { question = jsonQuestion.question, options = options })


type alias Options =
    List { text : String, isAnswer : Bool }


toOptions : String -> List String -> Maybe Options
toOptions answer options =
    -- TODO: refactor with Maybe.andThen or Maybe.map?
    case options of
        [ a, b, c, d ] ->
            if List.member answer options then
                Just
                    [ { text = a, isAnswer = a == answer }
                    , { text = b, isAnswer = b == answer }
                    , { text = c, isAnswer = c == answer }
                    , { text = d, isAnswer = d == answer }
                    ]

            else
                Nothing

        _ ->
            Nothing


type Topic
    = Html
    | Css
    | JavaScript
    | Accessibility


type alias TopicMetadata =
    { urlPath : String
    , logoSrc : String
    , displayName : String
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
