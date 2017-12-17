module EducationTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Education exposing (..)
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

        education =
            Education
                (createDate "10/1/1998")
                ((fromString >> Result.toMaybe) "09/30/2004")
                "TU-Ilmenau"
                "Medientechnologie"
                "Diplom Ingenieur (Medientechnologie)"
                2.1
                [ GraduationWork DIPLOMARBEIT "Erarbeitung einer Lernsoftware zum Themenkreis der Kommutierungssysteme von Kommutatormotoren" ]
                [ "Computergraphik"
                , "Bildverarbeitung"
                , "Internettechnologien"
                , "Applikationsprogrammierung"
                ]
    in
        describe "Eduction Module"
            [ describe "educationDecoder"
                [ test "should decode educationJson into Education datatype" <|
                    \_ ->
                        let
                            educationJson =
                                """
{
    "start": "1998-10-01T00:00:00.000+02:00",
    "end": "2004-09-30T00:00:00.000+02:00",
    "site": "TU-Ilmenau",
    "course": "Medientechnologie",
    "graduation": "Diplom Ingenieur (Medientechnologie)",
    "grade": 2.1,
    "works": [
        {
            "kind": "DIPLOMARBEIT",
            "title": "Erarbeitung einer Lernsoftware zum Themenkreis der Kommutierungssysteme von Kommutatormotoren"
        }
    ],
    "focals": [
        "Computergraphik",
        "Bildverarbeitung",
        "Internettechnologien",
        "Applikationsprogrammierung"
    ]
}
"""
                        in
                            case Decode.decodeString educationDecoder educationJson of
                                Ok decodedEducation ->
                                    Expect.equal education decodedEducation

                                Err error ->
                                    let
                                        _ =
                                            Debug.log "Error" (toString error)
                                    in
                                        Expect.fail "Decoding JSON as Education failed"
                , test "should decode educationJson without end date but with STUDIENARBEIT into Education datatype" <|
                    \_ ->
                        let
                            educationJson =
                                """
{
    "start": "1998-10-01T00:00:00.000+02:00",
    "end": null,
    "site": "TU-Ilmenau",
    "course": "Medientechnologie",
    "graduation": "Diplom Ingenieur (Medientechnologie)",
    "grade": 2.1,
    "works": [
        {
            "kind": "STUDIENARBEIT",
            "title": "Qualitätsorientierte Datenreduktion von dreidimensionalen Date mit Meshdecimation-Algorithmus"
        }
    ],
    "focals": [
        "Computergraphik",
        "Bildverarbeitung",
        "Internettechnologien",
        "Applikationsprogrammierung"
    ]
}
"""

                            studienarbeit =
                                GraduationWork
                                    STUDIENARBEIT
                                    "Qualitätsorientierte Datenreduktion von dreidimensionalen Date mit Meshdecimation-Algorithmus"
                        in
                            case Decode.decodeString educationDecoder educationJson of
                                Ok decodedEducation ->
                                    Expect.equal ({ education | end = Maybe.Nothing, works = [ studienarbeit ] }) decodedEducation

                                Err error ->
                                    Expect.fail "Decoding JSON as Education without end but with STUDIENARBEI failed"
                ]
            , describe "educationDescription"
                [ test "should render a timespan" <|
                    \_ ->
                        educationView (createDate "03/20/2010") education
                            |> Query.fromHtml
                            |> Query.has [ text "1998 - 2004" ]
                
                , test "should render graduation at site" <|
                    \_ ->
                        educationView (createDate "03/20/2010") education
                            |> Query.fromHtml
                            |> Query.has [ text "Medientechnologie an der TU-Ilmenau" ]
                
                , test "should render graduation with grade" <|
                    \_ ->
                        educationView (createDate "03/20/2010") education
                            |> Query.fromHtml
                            |> Query.has [ text "Abschluß: Diplom Ingenieur (Medientechnologie), Note: 2.1" ]
                
                , test "should render focals" <|
                    \_ ->
                        educationView (createDate "03/20/2010") education
                            |> Query.fromHtml
                            |> Query.has [ text "Schwerpunkte: Computergraphik, Bildverarbeitung, Internettechnologien, Applikationsprogrammierung"]
                ]
                , test "should render works" <|
                    \_ ->
                        educationView (createDate "03/20/2010") education
                            |> Query.fromHtml
                            |> Query.has [ text "Diplomarbeit: Erarbeitung einer Lernsoftware zum Themenkreis der Kommutierungssysteme von Kommutatormotoren"]
            ]
