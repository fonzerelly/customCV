module Education exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Date exposing (Date)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Timespan exposing (..)


type GraduationWorkKind
    = DIPLOMARBEIT
    | STUDIENARBEIT


type alias GraduationWork =
    { kind : GraduationWorkKind
    , title : String
    }


type alias Education =
    { start : Date
    , end : Maybe Date
    , site : String
    , course : String
    , graduation : String
    , grade : Float
    , works : List GraduationWork
    , focals : List String
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


kindDecoder : Decode.Decoder GraduationWorkKind
kindDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "DIPLOMARBEIT" ->
                        Decode.succeed DIPLOMARBEIT

                    "STUDIENARBEIT" ->
                        Decode.succeed STUDIENARBEIT

                    somethingElse ->
                        Decode.fail <| "Unknown theme: " ++ somethingElse
            )


workDecoder : Decode.Decoder GraduationWork
workDecoder =
    decode GraduationWork
        |> required "kind" kindDecoder
        |> required "title" Decode.string


educationDecoder : Decode.Decoder Education
educationDecoder =
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
        decode Education
            |> required "start" date
            |> required "end" (Decode.maybe date)
            |> required "site" Decode.string
            |> required "course" Decode.string
            |> required "graduation" Decode.string
            |> required "grade" Decode.float
            |> required "works" (Decode.list workDecoder)
            |> required "focals" (Decode.list Decode.string)


educationView : Date -> Education -> Html Msg
educationView currentDate education =
    let
        timespan = Timespan2 education.start education.end
        defaultRow attr txt = Grid.row attr
            [
                Grid.col [ Col.md12 ] [text txt]
            ]
        simpleRow = defaultRow []
        focals = String.concat <| List.intersperse ", " education.focals
        marginTop1 = Row.attrs [style [("margin-top", "1 em")]]
        marginBottom1 = Row.attrs [style [("margin-bottom", "1 em")]]

        renderWork: GraduationWork -> String
        renderWork work =
            let
                kind = case work.kind of
                    DIPLOMARBEIT -> "Diplomarbeit"
                    STUDIENARBEIT -> "Studienarbeit"
            in
                kind ++ ": " ++ work.title

    in
        Grid.row [] 
        [ Grid.col [ Col.md9, Col.pushMd3 ]
            [ Grid.container []
                (List.append 
                    [ simpleRow ( education.course ++ " an der " ++ education.site )
                    , simpleRow ( "Abschluß: " ++ education.graduation ++ ", Note: " ++ ( toString education.grade ) )
                    , defaultRow [ marginBottom1 ] ( "Schwerpunkte: " ++ focals)
                    , defaultRow [ marginTop1 ] ""
                    ]
                    ( List.map (simpleRow << renderWork) education.works )
                )
            ]
        , Grid.col [Col.md3, Col.pullMd9 ]
            [ text <| renderTimespan2 currentDate timespan]
        ]
