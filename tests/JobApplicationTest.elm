module JobApplicationTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import JobApplication exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, classes)
import Json.Decode as Decode
import Date exposing (..)

suite : Test
suite = 
    let
        applicationJson = """
        {
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
    in
        describe "applicationDecoder"
            [ test "should provide a list of urls to jobs" <|
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

