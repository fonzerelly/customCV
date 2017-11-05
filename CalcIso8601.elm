import Html exposing (..)
import Date exposing (..)
import Date.Extra.Format exposing (..)

type alias Model = {
    date: Date
}

type Msg = Msg1

createDate: String -> Date
createDate str =
    fromString str |> Result.withDefault (Date.fromTime 0)


initialModel: Model
initialModel = 
    { date= createDate "12/31/2008"
    }

view model =
    text <| isoString model.date

update msg model =
    case msg of
        Msg1 -> model

main = beginnerProgram 
    { view = view
    , update = update
    , model = initialModel
    }