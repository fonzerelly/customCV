module Main exposing (..)

import Html exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Date exposing (..)
import List

import JobDescription exposing (..)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

createDate: String -> Date
createDate str =
    fromString str |> Result.withDefault (Date.fromTime 0)

-- type alias Job =
--     { start : Date
--     , end : Date
--     , employer : String
--     , title : String
--     , tasks : List String
--     }

type Msg
    = Msg1
    | JobMsg JobDescription.Msg


type alias Model =
    { jobs : Job    }


init : ( Model, Cmd Msg )
init =
    ( Model (Job (createDate "1/1/2004") (createDate "12/31/2008") "Navigon AG" "Software-Engineer" ["Spracherkennung", "Oberflächenprogrammierung mit C++"]), Cmd.none )




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )

        JobMsg msg ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Html.map JobMsg (jobDescription 0 model.jobs)
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
