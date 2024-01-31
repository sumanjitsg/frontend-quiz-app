module Route exposing (Route(..), routeParser, toRoute, topicParser)

import Quiz
import Url exposing (Url)
import Url.Parser as Url



-- ROUTE


type Route
    = HomePage
    | TopicPage Quiz.Topic


routeParser : Url.Parser (Route -> a) a
routeParser =
    Url.oneOf
        [ Url.map HomePage Url.top
        , Url.map TopicPage topicParser
        ]


topicParser : Url.Parser (Quiz.Topic -> a) a
topicParser =
    Url.oneOf
        [ Url.map Quiz.Html (Url.s "html")
        , Url.map Quiz.Css (Url.s "css")
        , Url.map Quiz.JavaScript (Url.s "javascript")
        , Url.map Quiz.Accessibility (Url.s "accessibility")
        ]


toRoute : Url -> Route
toRoute url =
    case Url.parse routeParser url of
        Just route ->
            route

        Nothing ->
            -- TODO: redirect to HomePage
            HomePage
