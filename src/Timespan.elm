module Timespan exposing (..)

import Date exposing (Date, year, Month, month, day)
import Date.Extra.Facts exposing (monthNumberFromMonth)
import Time.DateTime exposing (DateTimeDelta, setDate, delta, zero, dateTime, DateTime)

type alias Timespan = {
    start : Date,
    end : Date
}

renderTimespan: Date -> Timespan -> String
renderTimespan currentDate timespan =
    let
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
        concatByDash (formatDate timespan.start) (formatDate timespan.end)
