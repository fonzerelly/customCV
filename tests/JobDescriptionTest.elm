module JobDescriptionTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import JobDescription exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, classes)

import Date exposing (..)

suite : Test
suite =
    let
        createDate: String -> Date
        createDate str =
            fromString str |> Result.withDefault (Date.fromTime 0)

        jobDescriptionWhereTimeDoesNotMatter = jobDescription 0
        job = (Job (createDate "1/1/2004") (createDate "12/31/2008") "Navigon AG" "Software-Engineer" ["Spracherkennung", "Oberflächenprogrammierung mit C++"])
    in     
        describe "jobDescription"
        [ describe "time range fromatting"
            [ describe "when job is older than two years"
                [ test "should render start year dash end year" <|
                    \_ -> jobDescription 3 job
                        |> Query.fromHtml
                        |> Query.find [ classes ["col-md-3", "pull-md-9"] ]
                        |> Query.has [ text "2004 - 2008"]
                ]
            , describe "when job is younger than two years"
                [ test "should render month and year of start and end" <|
                    \_ -> jobDescription 2 job
                        |> Query.fromHtml
                        |> Query.find [ classes ["col-md-3", "pull-md-9"] ]
                        |> Query.has [ text "01/2004 - 12/2008"]
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
    