module JobApplicationTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import JobApplication exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, classes, style)
import Json.Decode as Decode
import Date exposing (..)

suite : Test
suite = 
    let
        applicationJson = """
        {
            "letterParagraphs": [
                "Sehr geehrte Frau Mustermann",
                "ich will für Sie arbeiten."
            ],
            "jobLinks": [
                "data/job/navigon.json",
                "data/job/elektrobit.json"
            ],
            "educationLinks": [
                "data/education/tu-ilmenau.json"
            ],
            "miscLinks": [
                "data/misc/auslandsaufenthalte.json"
            ]
        }
        """
        application = JobApplication
            [ "Sehr geehrte Frau Mustermann,"
            , "ich will für Sie arbeiten."
            ]
            []
            []
            []
    in
        describe "JobApplication Module"
        [ describe "applicationDecoder"
                [ test "should provide an application letter" <|
                    \_ ->
                        case Decode.decodeString applicationDecoder applicationJson of
                            Ok app -> Expect.equal app.letterParagraphs ["Sehr geehrte Frau Mustermann", "ich will für Sie arbeiten."]
                            Err err -> Expect.fail ("Decoding application failed due to: " ++ (toString err))

                , test "should provide a list of urls to jobs" <|
                    \_ ->
                        case Decode.decodeString applicationDecoder applicationJson of
                            Ok app -> Expect.equal app.jobLinks ["data/job/navigon.json", "data/job/elektrobit.json"]
                            Err err -> Expect.fail ("Decoding application failed due to: " ++ (toString err))

                , test "should provide a list of urls to educations" <|
                    \_ ->
                        case Decode.decodeString applicationDecoder applicationJson of
                            Ok app -> Expect.equal app.educationLinks ["data/education/tu-ilmenau.json"]
                            Err err -> Expect.fail ("Decoding application failed due to: " ++ (toString err))

                , test "should provide a list of urls to miscs" <|
                    \_ ->
                        case Decode.decodeString applicationDecoder applicationJson of
                            Ok app -> Expect.equal app.miscLinks ["data/misc/auslandsaufenthalte.json"]
                            Err err -> Expect.fail ("Decoding application failed due to: " ++ (toString err))

                ]
        , describe "applicationView"
                [ test "should render all letter paragraphs in new rows visually disticted" <|
                    \_ ->
                        let
                            checkText i txt markup =
                                markup
                                    |> Query.fromHtml
                                    |> Query.findAll [ style [ ("margin-bottom", "1em") ] ]
                                    |> Query.index i
                                    |> Query.has [ text txt ]
                        in
                            Expect.all
                                [ checkText 0 "Sehr geehrte Frau Mustermann,"
                                , checkText 1 "ich will für Sie arbeiten."
                                ]
                                ( applicationView application )

                , test "should render good by on the right side" <|
                    \_ ->
                        let
                            checkText i txt markup =
                                markup
                                    |> Query.fromHtml
                                    |> Query.findAll [ style [ ("text-align", "right") ] ]
                                    |> Query.index i
                                    |> Query.has [ text txt ]
                        in
                            Expect.all
                                [ checkText 0 "Mit freundlichen Grüßen"
                                , checkText 1 "Christian Hörauf"
                                ]
                                ( applicationView application )
                ]
        ]

