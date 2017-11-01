module JobDescriptionTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Types exposing (..)
import JobDescription exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, classes)

suite : Test
suite =
    let
        job = (Job (createDate "1.1.2004") (createDate "31.12.2008") "Navigon AG" "Software-Engineer" ["Spracherkennung", "Oberflächenprogrammierung mit C++"])
    in     
        describe "jobDescription"
        [ test "should combine employer and title" <|
            \_ -> jobDescription job
                |> Query.fromHtml
                |> Query.find [classes ["col-md-9", "push-md-3"] ]
                |> Query.has [text "Software-Engineer bei Navigon AG"]
        , describe "tasks" 
            [ test "should render as many li's as tasks" <|
                \_ -> jobDescription job
                    |> Query.fromHtml
                    |> Query.findAll [ tag "li" ]
                    |> Query.count (Expect.equal <| List.length job.tasks)                   
            , describe "li's"
                [ test "should contain first task" <|
                    \_ -> jobDescription job
                        |> Query.fromHtml
                        |> Query.findAll [ tag "li" ]
                        |> Query.first
                        |> Query.has [ text "Spracherkennung" ]
                , test "should contain Second task" <|
                    \_ -> jobDescription job
                        |> Query.fromHtml
                        |> Query.findAll [ tag "li" ]
                        |> Query.index 1
                        |> Query.has [ text "Oberflächenprogrammierung mit C++" ]
                ]
            ]
        ]
    