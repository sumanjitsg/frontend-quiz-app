module Api exposing (JsonQuestion, JsonQuizVault, JsonTopicQuestions, getVaultData, questionDecoder, quizVaultDecoder, responseDecoder, topicQuestionsDecoder)

import Dict exposing (Dict)
import Http
import Json.Decode as Json



-- HTTP


getVaultData msg =
    Http.get
        { url = "/data.json"
        , expect = Http.expectJson msg responseDecoder
        }



-- DECODERS


type alias JsonQuizVault =
    Dict String JsonTopicQuestions


type alias JsonTopicQuestions =
    List JsonQuestion


type alias JsonQuestion =
    { question : String
    , options : List String
    , answer : String
    }


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
