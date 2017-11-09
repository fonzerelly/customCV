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
        createDate: String -> Date
        createDate str =
            fromString str |> Result.withDefault (Date.fromTime 0)

        jobDescriptionWhereTimeDoesNotMatter = jobDescription (createDate "11/04/2017")
        job = (Job 
            (createDate "1/1/2004") 
            (createDate "12/31/2008") 
            "Navigon AG" 
            "Software-Engineer" 
            [ "Spracherkennung"
            , "Oberflächenprogrammierung mit C++"
            ])

        
    in  
        describe "JobDescription Module"
        [   
            describe "jobDescription"
            [ describe "time range fromatting"
                [ describe "when job is older than 12 months"
                    [ test "should render start year dash end year" <|
                        \_ -> jobDescription (createDate "03/20/2010") job
                            |> Query.fromHtml
                            |> Query.find [ classes ["col-md-3", "pull-md-9"] ]
                            |> Query.has [ text "2004 - 2008"]
                    ]
                , describe "when job is younger than 12 months"
                    [ test "should render month and year for latest date of start and end" <|
                        \_ -> jobDescription (createDate "03/20/2009") job
                            |> Query.fromHtml
                            |> Query.find [ classes ["col-md-3", "pull-md-9"] ]
                            |> Query.has [ text "2004 - 12/2008"]
                    ]
                , describe "when job was shorter than 12 months"
                    [ test "should render month and year for each date" <|
                        \_ -> jobDescription (createDate "03/20/2009") {job | start = createDate "10/01/2008"}
                            |> Query.fromHtml
                            |> Query.find [ classes ["col-md-3", "pull-md-9"] ]
                            |> Query.has [text "10/2008 - 12/2008"]
                    ]
                , describe "when job ends at the same date as the currentDate" <|
                    [ test "should render \"heute\" instead of date" <|
                        \_ -> jobDescription (createDate "12/31/2008") job
                            |> Query.fromHtml
                            |> Query.find [ classes ["col-md-3", "pull-md-9"] ]
                            |> Query.has [text "2004 - heute"]
                    ]
                ]
            , test "should combine employer and title" <|
                \_ -> jobDescriptionWhereTimeDoesNotMatter job
                    |> Query.fromHtml
                    |> Query.find [classes ["col-md-9", "push-md-3"] ]
                    |> Query.has [text "Software-Engineer bei Navigon AG"]
            , describe "tasks" 
                [ test "should render as many li's as tasks" <|
                    \_ -> jobDescriptionWhereTimeDoesNotMatter job
                        |> Query.fromHtml
                        |> Query.findAll [ tag "li" ]
                        |> Query.count (Expect.equal <| List.length job.tasks)                   
                , describe "li's"
                    [ test "should contain first task" <|
                        \_ -> jobDescriptionWhereTimeDoesNotMatter job
                            |> Query.fromHtml
                            |> Query.findAll [ tag "li" ]
                            |> Query.first
                            |> Query.has [ text "Spracherkennung" ]
                    , test "should contain Second task" <|
                        \_ -> jobDescriptionWhereTimeDoesNotMatter job
                            |> Query.fromHtml
                            |> Query.findAll [ tag "li" ]
                            |> Query.index 1
                            |> Query.has [ text "Oberflächenprogrammierung mit C++" ]
                    ]
                ]
            ]
        , describe "jobDecoder"
            [ test  "should decode JobJson into Job datatype"  <|
                \_ -> 
                    let
                        jobDecoder = createJobDecoder (createDate "12/31/2008")
                        jobJson = """
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
                            Ok decodedJob -> Expect.equal job decodedJob
                            Err error -> Expect.fail "Decoding JSON as Job failed"
            
            , test "should set passed in current date as end date when end date is null" <|
                \_->
                    let 
                        jobDecoder = createJobDecoder (createDate "03/20/2009") 
                        jobJson = """
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
                            Ok decodedJob -> Expect.equal {job| end = createDate "03/20/2009"} decodedJob
                            Err error -> Expect.fail "Decoding JSON as Job with end = null failed"
            ]
        ]