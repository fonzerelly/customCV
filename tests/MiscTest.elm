module MiscTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Misc exposing (..)
import Timespan exposing (..)
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

        auslandsaufenthalte =
            (Misc
                "Auslandsaufenthalte"
                [ LiveSection 
                    [ Timespan
                        (createDate "3/1/2007")
                        Nothing
                    ]
                    [ "Technische Unterstützung des US-Standorts von Navigon vor Ort in Chicago"
                    ]
                , LiveSection
                    [ Timespan
                        (createDate "6/1/1992")
                        (Just <| createDate "6/30/1992")
                    , Timespan
                        (createDate "7/1/1993")
                        (Just <| createDate "8/18/1993")
                    ]
                    [ "Sparchschule des Jugendwerks der Arbeiterwohlfahrt in Bournemouth, England" ]
                ]
            )

        schulbildung =
            (Misc
                "Schulbildung"
                [ LiveSection
                    [ Timespan
                        (createDate "9/1/1984")
                        (Just <| createDate "8/30/1997")
                    ]
                    [ "Grundschule in Schwabach"
                    , "Adam-Kraft-Gymnasium in Schwabach: "
                    , "Allgemeine Hochschulreife, Note 3.2"
                    ]
                ]
            )
    in
        describe "Misc Module"
            [ describe "miscView"
                [ test "should render title Schulbildung" <|
                    \_ -> 
                        miscView (createDate "03/20/2010") schulbildung
                            |> Query.fromHtml
                            |> Query.find [ tag "h3" ]
                            |> Query.has [ text "Schulbildung" ]
                
                , test "should render each description in a new row " <|
                    \_ ->
                        let
                            checkText i txt markup =
                                markup
                                    |> Query.fromHtml
                                    |> Query.findAll [ classes [ "col-md-9" ] ]
                                    |> Query.index i
                                    |> Query.has [ text txt ]
                        in
                            Expect.all 
                                [ checkText 0 "Grundschule in Schwabach"
                                , checkText 1 "Adam-Kraft-Gymnasium in Schwabach: "
                                , checkText 2 "Allgemeine Hochschulreife, Note 3.2"
                                ]
                                ( miscView (createDate "03/20/2009") schulbildung )

                , test "should render emtpy timespans for unmatched descriptions " <|
                    \_ ->
                        let
                            checkText i txt markup =
                                markup
                                    |> Query.fromHtml
                                    |> Query.findAll [ classes [ "col-md-3" ] ]
                                    |> Query.index i
                                    |> Query.has [ text txt ]
                        in
                            Expect.all 
                                [ checkText 0 "1984 - 1997"
                                , checkText 1 ""
                                , checkText 2 ""
                                ]
                                ( miscView (createDate "03/20/2009") schulbildung )
                
                , test "should render each timespan in a new row " <|
                    \_ ->
                        let
                            checkText i txt markup =
                                markup
                                    |> Query.fromHtml
                                    |> Query.findAll [ classes [ "col-md-3" ] ]
                                    |> Query.index i
                                    |> Query.has [ text txt ]
                        in
                            Expect.all 
                                [ checkText 0 "2007 - heute"
                                , checkText 1 "06/1992"
                                , checkText 2 "07 - 08/1993"
                                ]
                                ( miscView (createDate "03/20/2009") auslandsaufenthalte )
                ]
            , describe "miscDecoder"
                [ test "should decode schulbildungJson into Misc datatype" <|
                    \_ ->
                        let
                            schulbildungJson = 
                            """
{
    "title": "Schulbildung",
    "liveSections": [
        {
            "timespans": [
                {
                    "start": "1984-09-01T00:00:00.000+02:00",
                    "end": "1997-08-30T00:00:00.000+02:00"
                }
            ],
            "descriptions": [
                "Grundschule in Schwabach",
                "Adam-Kraft-Gymnasium in Schwabach: ",
                "Allgemeine Hochschulreife, Note 3.2"
            ]
        }
    ]
}
                            """
                        in
                            case Decode.decodeString miscDecoder schulbildungJson of
                                Ok decodedSchulbildungen ->
                                    Expect.equal schulbildung decodedSchulbildungen

                                Err error ->
                                    Expect.fail "Decoding JSON as Job failed"
                    
                , test "should decode miscJson into Misc datatype" <|
                    \_ ->
                        let
                            auslandsaufenthalteJson =
                                """
{
    "title": "Auslandsaufenthalte",
    "liveSections": [
        {
            "timespans": [
                {
                    "start": "2007-03-01T00:00:00.000+01:00",
                    "end": null
                }
            ],
            "descriptions": [
                "Technische Unterstützung des US-Standorts von Navigon vor Ort in Chicago"
            ]
        },
        {
            "timespans": [
                {
                    "start": "1992-06-01T00:00:00.000+02:00",
                    "end": "1992-06-30T00:00:00.000+02:00"
                },
                {
                    "start": "1993-07-01T00:00:00.000+02:00",
                    "end": "1993-08-18T00:00:00.000+02:00"
                }
            ],
            "descriptions": [
                "Sparchschule des Jugendwerks der Arbeiterwohlfahrt in Bournemouth, England"
            ]
        }
    ]
}
"""
                        in
                            case Decode.decodeString miscDecoder auslandsaufenthalteJson of
                                Ok decodedAuslandsaufenthalte ->
                                    Expect.equal auslandsaufenthalte decodedAuslandsaufenthalte

                                Err error ->
                                    Expect.fail "Decoding JSON as Job failed"
                ]
            ]
