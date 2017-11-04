module JobDescription exposing (..)

import Html exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Date exposing (Date, year, Month, month)
import Date.Extra.Facts exposing (monthNumberFromMonth)
-- import Time.DateTime (DateTimeDelta, )

type alias Job =
    { start : Date
    , end : Date
    , employer : String
    , title : String
    , tasks : List String
    }

type Msg = Init

-- According to documentation this is still missing.
-- We will need that as soon as we have to really handle
-- messages in JobDescription

-- type alias Model =
-- { job: Job
-- }

-- initialModel: Model
-- initialModel = { job = Job (createDate "1/1/2004") (createDate "12/31/2008") "Navigon AG" "Software-Engineer" ["Spracherkennung", "Oberflächenprogrammierung mit C++"])
-- }

-- update: Msg -> Model -> (Model, Cmd Msg)
-- update msg model =
--     case msg of
--         Init ->
--             (model, Cmd.none)

jobDescription : Int -> Job -> Html Msg
jobDescription yearsPassedSince job =
    let 
        entry:String -> Html Msg
        entry task = li [] [text task]

        yearToString: Date -> String
        yearToString = toString << year

        concatDates: (Date -> String) -> Date -> Date -> String
        concatDates dateToStrFn start end =
            (dateToStrFn start) ++ " - " ++ (dateToStrFn end)

        monthYearToString: Date -> String
        monthYearToString d =
            let
                monthToString: Month -> String
                monthToString m =
                    let
                        monthNum = monthNumberFromMonth m
                    in
                        if monthNum < 10 then "0" ++ toString monthNum else toString monthNum
            in
                (monthToString <| month d) ++ "/" ++ (yearToString d)

        timeFn = if (yearsPassedSince > 2) then concatDates yearToString else concatDates monthYearToString
    in
        Grid.row []
            [ Grid.col 
                [ Col.md9, Col.pushMd3 ]
                [ text (job.title ++ " bei " ++ job.employer)
                , ul [] (List.map entry job.tasks)
                ]
            , Grid.col
                [ Col.md3, Col.pullMd9 ]
                [ text <| timeFn job.start job.end ]
            ]