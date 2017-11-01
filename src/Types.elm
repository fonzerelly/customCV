module Types exposing (..)

import Date exposing (..)


type alias Job =
    { start : Date
    , end : Date
    , employer : String
    , title : String
    , tasks : List String
    }

createDate: String -> Date
createDate str =
    fromString str |> Result.withDefault (Date.fromTime 0)

type Msg
    = Msg1

