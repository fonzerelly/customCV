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
            ]

