module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Http
import Json.Decode as Decode exposing (Decoder)
import Url
import Url.Parser as Parser exposing (Parser)


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
    ( Model (toRoute url) key, getQuizzes )



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
                Ok _ ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view { route } =
    { title = "Frontend Quiz App"
    , body = [ viewHeader route, viewMain route ]
    }


viewHeader : Route -> Html Msg
viewHeader route =
    header
        [ class "container body__header" ]
        [ nav []
            [ case route of
                HomePage ->
                    text ""

                TopicPage topicName ->
                    case toTopic topicName of
                        Just topic ->
                            div
                                [ class "topic-info text--medium" ]
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


viewMain : Route -> Html Msg
viewMain route =
    main_
        [ class "container" ]
        (case route of
            HomePage ->
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

                -- List of quiz topics
                , ul
                    [ class "list text--medium" ]
                    (List.map
                        (\topic ->
                            li []
                                [ a
                                    [ href topic.urlPath
                                    , class "list__item topic-info"
                                    ]
                                    [ img
                                        [ src topic.logoSrc ]
                                        []
                                    , span []
                                        [ text topic.displayName ]
                                    ]
                                ]
                        )
                        topics
                    )
                ]

            TopicPage _ ->
                [ text "" ]
        )



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


responseDecoder : Decoder Quizzes
responseDecoder =
    Decode.field "quizzes" quizzesDecoder


quizzesDecoder : Decoder Quizzes
quizzesDecoder =
    Decode.map Dict.fromList <|
        Decode.list quizDecoder


quizDecoder : Decoder ( String, List Question )
quizDecoder =
    Decode.map2
        (\topicName questions ->
            ( topicName, questions )
        )
        (Decode.field "title" Decode.string)
        (Decode.field "questions" <|
            Decode.list questionDecoder
        )


questionDecoder : Decoder Question
questionDecoder =
    Decode.map3
        Question
        (Decode.field "question" Decode.string)
        (Decode.field "options" <|
            Decode.list Decode.string
        )
        (Decode.field "answer" Decode.string)


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
