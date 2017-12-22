module JobDescription exposing (..)

import Html exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Date exposing (Date)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Timespan exposing (..)


type alias Job =
    { start : Date
    , end : Maybe Date
    , employer : String
    , title : String
    , tasks : List String
    }


type Msg
    = Init



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
        entry : String -> Html Msg
        entry task =
            li [] [ text task ]

        timespan =
            Timespan2 job.start job.end
    in
        Grid.row []
            [ Grid.col
                [ Col.md9, Col.pushMd3 ]
                [ text (job.title ++ " bei " ++ job.employer)
                , ul [] (List.map entry job.tasks)
                ]
            , Grid.col
                [ Col.md3, Col.pullMd9 ]
                [ text <| renderTimespan2 currentDate timespan ]
            ]

jobDecoder : Decode.Decoder Job
jobDecoder =
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
            |> required "end" (Decode.maybe date)
            |> required "employer" Decode.string
            |> required "title" Decode.string
            |> required "tasks" (Decode.list Decode.string)
