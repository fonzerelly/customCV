module JobDescriptionTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import JobDescription exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, classes)
import Json.Decode as Decode
import Date exposing (..)


suite : Test
suite =
    let
        createDate : String -> Date
        createDate str =
            fromString str |> Result.withDefault (Date.fromTime 0)

        jobDescriptionWhereTimeDoesNotMatter =
            jobDescription (createDate "11/04/2017")

        job =
            (Job
                (createDate "1/1/2004")
                (Just <| createDate "12/31/2008")
                "Navigon AG"
                "Software-Engineer"
                [ "Spracherkennung"
                , "Oberflächenprogrammierung mit C++"
                ]
            )
    in
        describe "JobDescription Module"
            [ describe "jobDescription"
                [  test "should render a timerange" <|
                    \_ ->
                        jobDescription (createDate "03/20/2010") job
                            |> Query.fromHtml
                            |> Query.find [ classes [ "col-12", "col-md-3" ] ]
                            |> Query.has [ text "2004 - 2008" ]

                , test "should combine employer and title" <|
                    \_ ->
                        jobDescriptionWhereTimeDoesNotMatter job
                            |> Query.fromHtml
                            |> Query.find [ classes [ "col-12", "col-md-9" ] ]
                            |> Query.has [ text "Software-Engineer bei Navigon AG" ]

                , describe "tasks"
                    [ test "should render as many li's as tasks" <|
                        \_ ->
                            jobDescriptionWhereTimeDoesNotMatter job
                                |> Query.fromHtml
                                |> Query.findAll [ tag "li" ]
                                |> Query.count (Expect.equal <| List.length job.tasks)
                    , describe "li's"
                        [ test "should contain first task" <|
                            \_ ->
                                jobDescriptionWhereTimeDoesNotMatter job
                                    |> Query.fromHtml
                                    |> Query.findAll [ tag "li" ]
                                    |> Query.first
                                    |> Query.has [ text "Spracherkennung" ]
                        , test "should contain Second task" <|
                            \_ ->
                                jobDescriptionWhereTimeDoesNotMatter job
                                    |> Query.fromHtml
                                    |> Query.findAll [ tag "li" ]
                                    |> Query.index 1
                                    |> Query.has [ text "Oberflächenprogrammierung mit C++" ]
                        ]
                    ]
                ]
            , describe "jobDecoder"
                [ test "should decode JobJson into Job datatype" <|
                    \_ ->
                        let
                            jobJson =
                                """
{
    "start": "2004-01-01T00:00:00.000+01:00",
    "end": "2008-12-31T00:00:00.000+01:00",
    "employer": "Navigon AG",
    "title": "Software-Engineer",
    "tasks": [
        "Spracherkennung",
        "Oberflächenprogrammierung mit C++"
    ]
}
"""
                        in
                            case Decode.decodeString jobDecoder jobJson of
                                Ok decodedJob ->
                                    Expect.equal job decodedJob

                                Err error ->
                                    Expect.fail "Decoding JSON as Job failed"
                , test "should set passed in current date as end date when end date is null" <|
                    \_ ->
                        let
                            jobJson =
                                """
{
    "start": "2004-01-01T00:00:00.000+01:00",
    "end": null,
    "employer": "Navigon AG",
    "title": "Software-Engineer",
    "tasks": [
        "Spracherkennung",
        "Oberflächenprogrammierung mit C++"
    ]
}
"""
                        in
                            case Decode.decodeString jobDecoder jobJson of
                                Ok decodedJob ->
                                    Expect.equal { job | end = Nothing } decodedJob

                                Err error ->
                                    Expect.fail "Decoding JSON as Job with end = null failed"
                ]
            ]
