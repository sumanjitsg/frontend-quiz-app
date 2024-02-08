module Main exposing (main)

import Api
import Browser
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Http
import Quiz exposing (Quiz)
import Route exposing (Route)
import Url exposing (Url)



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
    , navigationKey : Navigation.Key
    , quiz : Quiz
    }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url navigationKey =
    ( Model (Route.toRoute url) navigationKey initialQuiz, Api.getVaultData ReceivedVaultData )



-- UPDATE


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | ReceivedVaultData (Result Http.Error Api.JsonQuizVault)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.navigationKey <|
                        Url.toString url
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        UrlChanged url ->
            ( { model | route = Route.toRoute url }
            , Cmd.none
            )

        ReceivedVaultData data ->
            case data of
                Ok jsonQuizVault ->
                    let
                        updatedVault =
                            Quiz.updateQuizVault jsonQuizVault model.quiz.vault

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


view : Model -> Browser.Document Msg
view model =
    { title = "Frontend Quiz App"
    , body = [ viewHeader model, viewMain model ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    header
        [ class "container body__header" ]
        [ nav []
            [ case model.route of
                Route.HomePage ->
                    text ""

                Route.TopicPage topic ->
                    case Dict.get (Quiz.toTopicString topic) model.quiz.metadata of
                        Just topicInfo ->
                            div
                                [ class "header__image-text text--medium" ]
                                [ img
                                    [ src topicInfo.logoSrc ]
                                    []
                                , span []
                                    [ text topicInfo.displayName ]
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
            Route.HomePage ->
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
                                    Quiz.toTopicString topic
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

            Route.TopicPage topic ->
                case Dict.get (Quiz.toTopicString topic) model.quiz.vault of
                    Just topicQuestions ->
                        case topicQuestions.current of
                            Just question ->
                                case topicQuestions.currentIndex of
                                    Just currentIndex ->
                                        [ div []
                                            [ p [ class "text--italic" ]
                                                [ text ("Question " ++ String.fromInt currentIndex ++ " of " ++ String.fromInt (1 + List.length topicQuestions.next)) ]
                                            , p
                                                [ class "text--large" ]
                                                [ text question.question
                                                ]
                                            ]
                                        , div
                                            [ class "text--medium" ]
                                            [ ul [ class "list" ] <|
                                                let
                                                    toUpperLetter index =
                                                        String.fromChar <| Char.fromCode (65 + index)

                                                    indexedOptions =
                                                        List.indexedMap
                                                            (\index option ->
                                                                Tuple.pair (toUpperLetter index) option
                                                            )
                                                            question.options
                                                in
                                                List.map
                                                    (\( index, option ) ->
                                                        li [ class "list-item" ]
                                                            [ div []
                                                                [ span []
                                                                    [ text index ]
                                                                , span [] [ text option.text ]
                                                                ]
                                                            ]
                                                    )
                                                    indexedOptions
                                            , button
                                                [ class "btn btn--primary" ]
                                                [ text "Submit Answer" ]
                                            ]
                                        ]

                                    Nothing ->
                                        -- Debug.todo "no current index"
                                        [ text "" ]

                            Nothing ->
                                -- Debug.todo "no current question"
                                [ text "" ]

                    Nothing ->
                        -- Debug.todo "no topic questions"
                        [ text "" ]



-- TODO: what do we show on topic page if topic vault opened directly and there's an error populating the topic vault?
-- QUIZ


initialTopicQuestions : Quiz.TopicQuestions
initialTopicQuestions =
    { current = Nothing
    , next = []
    , currentIndex = Nothing
    }


initialQuiz : Quiz
initialQuiz =
    { topics = [ Quiz.Html, Quiz.Css, Quiz.JavaScript, Quiz.Accessibility ]
    , vault =
        Dict.fromList
            [ ( Quiz.toTopicString Quiz.Html, initialTopicQuestions )
            , ( Quiz.toTopicString Quiz.Css, initialTopicQuestions )
            , ( Quiz.toTopicString Quiz.JavaScript, initialTopicQuestions )
            , ( Quiz.toTopicString Quiz.Accessibility, initialTopicQuestions )
            ]
    , metadata =
        Dict.fromList
            [ ( Quiz.toTopicString Quiz.Html
              , { urlPath = "/html"
                , logoSrc = "/assets/images/icon-html.svg"
                , displayName = "HTML"
                }
              )
            , ( Quiz.toTopicString Quiz.Css
              , { urlPath = "/css"
                , logoSrc = "/assets/images/icon-css.svg"
                , displayName = "CSS"
                }
              )
            , ( Quiz.toTopicString Quiz.JavaScript
              , { urlPath = "/javascript"
                , logoSrc = "/assets/images/icon-js.svg"
                , displayName = "JavaScript"
                }
              )
            , ( Quiz.toTopicString Quiz.Accessibility
              , { urlPath = "/accessibility"
                , logoSrc = "/assets/images/icon-accessibility.svg"
                , displayName = "Accessibility"
                }
              )
            ]
    }



{- TODO: split urlPath and check if last one is equal to topicName -}
