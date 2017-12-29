module Timespan exposing (..)

import Date exposing (Date, year, Month, month, day)
import Date.Extra.Facts exposing (monthNumberFromMonth)
import Time.DateTime exposing (DateTimeDelta, setDate, delta, zero, dateTime, DateTime)

type alias Timespan = {
    start : Date,
    end : Maybe Date
}

renderTimespan: Date -> Timespan -> String
renderTimespan currentDate timespan =
    let
        yearToString: Date -> String
        yearToString = toString << year

        concatByDash: String -> String -> String
        concatByDash start end =
            start ++ " - " ++ end

        monthToString: Month -> String
        monthToString m =
            let
                monthNum = monthNumberFromMonth m
            in
                if monthNum < 10 then "0" ++ toString monthNum else toString monthNum

        monthYearToString: Date -> String
        monthYearToString d =
                (monthToString <| month d) ++ "/" ++ (yearToString d)

        maybeMonthYearToString: Maybe Date -> String
        maybeMonthYearToString maybeDate =
            case maybeDate of
                Just date -> monthYearToString date
                Nothing -> "heute"

        toDateTime: Date -> DateTime
        toDateTime date = dateTime { zero| year = (year date), month = (monthNumberFromMonth <| month date), day = (day date) }
    
        formatDate: Maybe Date -> String
        formatDate maybeDate =
            case maybeDate of
                Just date ->
                    let
                        deltaSinceDate = delta (toDateTime currentDate) (toDateTime date)
                    in
                        if deltaSinceDate.months > 12 then yearToString date else monthYearToString date

                Nothing -> "heute"
        
        maybeSameYear = Maybe.map2 (\start end -> (year start) == (year end)) (Just timespan.start) timespan.end 
        maybeSameMonth = Maybe.map2 (\start end -> (month start) == (month end)) (Just timespan.start) timespan.end

        defaultResult = concatByDash (formatDate <| Just timespan.start) (formatDate timespan.end)
    in
        case maybeSameYear of
            Just sameYear ->
                if sameYear then
                    case maybeSameMonth of
                        Just sameMonth ->
                            if sameMonth then
                                maybeMonthYearToString timespan.end
                            else
                                concatByDash (monthToString <| month timespan.start) (maybeMonthYearToString timespan.end)
                        Nothing ->
                            defaultResult
                else
                    defaultResult
            Nothing -> 
                defaultResult
