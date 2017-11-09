module JobDescription exposing (..)

import Html exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Date exposing (Date, year, Month, month, day)
import Date.Extra.Facts exposing (monthNumberFromMonth)
import Time.DateTime exposing (DateTimeDelta, setDate, delta, zero, dateTime, DateTime)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)

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

jobDescription : Date -> Job -> Html Msg
jobDescription currentDate job =
    let 
        entry:String -> Html Msg
        entry task = li [] [text task]

        yearToString: Date -> String
        yearToString = toString << year

        concatByDash: String -> String -> String
        concatByDash start end =
            start ++ " - " ++ end

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

        toDateTime: Date -> DateTime
        toDateTime date = dateTime { zero| year = (year date), month = (monthNumberFromMonth <| month date), day = (day date) }

        formatDate: Date -> String
        formatDate date =
            let
               deltaSinceDate = delta (toDateTime currentDate) (toDateTime date)
            in
                if ( date == currentDate ) then
                    "heute"
                else
                    if deltaSinceDate.months > 12 then yearToString date else monthYearToString date
    in
        Grid.row []
            [ Grid.col 
                [ Col.md9, Col.pushMd3 ]
                [ text (job.title ++ " bei " ++ job.employer)
                , ul [] (List.map entry job.tasks)
                ]
            , Grid.col
                [ Col.md3, Col.pullMd9 ]
                [ text <| (concatByDash (formatDate job.start) (formatDate job.end)) ]
            ]



createJobDecoder : Date -> Decode.Decoder Job
createJobDecoder currentDate =
    let
    
        date : Decode.Decoder Date
        date =
            let
                convert : String -> Decode.Decoder Date
                convert raw =
                    case Date.fromString raw of
                        Ok date ->
                            Decode.succeed date

                        Err error ->
                            Decode.fail error
            in
                Decode.string |> Decode.andThen convert
    in
    decode Job
        |> required "start" date
        |> optional "end" date currentDate
        |> required "employer" Decode.string
        |> required "title" Decode.string
        |> required "tasks" (Decode.list Decode.string)