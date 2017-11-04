module Main exposing (..)

import Html exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Date exposing (..)
import Task
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

type Msg
    = Msg1
    | SetDate (Maybe Date)
    | JobMsg JobDescription.Msg


type alias Model =
    { jobs : Job
    , currentDate: Maybe Date
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (Job (createDate "1/1/2004") (createDate "12/31/2008") "Navigon AG" "Software-Engineer" ["Spracherkennung", "Oberflächenprogrammierung mit C++"])
        Nothing
    , now )

now : Cmd Msg
now = Task.perform (Just >> SetDate) Date.now



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )

        JobMsg msg ->
            ( model, Cmd.none )

        SetDate date ->
            ( {model | currentDate = date}, Cmd.none )


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , case model.currentDate of
            Just date -> Html.map JobMsg (jobDescription date model.jobs)
            Nothing -> text ""
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
