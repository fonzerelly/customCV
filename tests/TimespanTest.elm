module TimespanTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Timespan exposing (..)
import Date exposing (..)

suite : Test
suite = 
    let
        createDate: String -> Date
        createDate str =
            fromString str |> Result.withDefault (Date.fromTime 0)

        timespan = Timespan (createDate "1/1/2004") (Just <| createDate "12/31/2008")
    in
        describe "Timespan"
        [ describe "renderTimespan" 
            [
                describe "when Timespan is older than 12 months"
                [ test "should render start year dash end year" <|
                    \_ ->
                        let
                            currentDate = createDate "03/20/2010"

                        in
                            Expect.equal "2004 - 2008"
                                (renderTimespan currentDate timespan)

                , describe "when Timespan is inside of the same year"
                    [ test "should render month and year for each date" <|
                        \_ ->
                            let
                                currentDate = createDate "03/20/2010"
                                startDate = createDate "08/01/2008"

                            in
                                Expect.equal "08 - 12/2008"
                                    (renderTimespan currentDate (Timespan startDate (Just <| createDate "12/31/2008")))
                    ]
                ],

                describe "when Timespan is younger than 12 months"
                [ test "should render month and year for latest start and end" <|
                    \_->
                        let
                            currentDate = createDate "03/20/2009"
                        in
                            Expect.equal "2004 - 12/2008"
                                (renderTimespan currentDate timespan)

                , describe "when Timespan was shorter than 12 months" 
                    [ test "should render month and year for each date" <|
                        \_ -> 
                            let
                                currentDate = createDate "03/20/2009"
                                startDate = createDate "10/01/2008"
                                endDate = createDate "02/28/2009"
                            in
                                Expect.equal "10/2008 - 02/2009"
                                    (renderTimespan currentDate (Timespan startDate (Just endDate)))
                    
                    , describe "when Timespan was shorter than a month"
                        [ test "should render only one month and the year" <|
                            \_ ->
                                let
                                    currentDate = createDate "03/20/2009"
                                    startDate = createDate "10/01/2008"
                                    endDate = createDate "10/28/2008"
                                in
                                    Expect.equal "10/2008"
                                        (renderTimespan currentDate (Timespan startDate (Just endDate)))

                        ]
                    ]
                ],

                describe "when Timespan end is Nothing" <|
                [ test "should render \"heute\" instead of date" <|
                    \_ ->
                        let
                            currentDate = createDate "12/31/2008"
                        in
                            Expect.equal "2004 - heute"
                                (renderTimespan currentDate {timespan| end = Maybe.Nothing})
                ]
            ]
        ]