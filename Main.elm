module Main exposing (..)

import Html exposing (..)
import Http exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Date exposing (..)
import Task
import List
import Json.Decode as Decode
import JobDescription exposing (..)
import JobApplication exposing (..)
import Platform.Cmd exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


createDate : String -> Date
createDate str =
    fromString str |> Result.withDefault (Date.fromTime 0)


type Msg
    = DataLoaded (Result Http.Error Job)
    | SetDate (Maybe Date)
    | JobMsg JobDescription.Msg



-- Todo:
-- So what we need is the Date in combination with the application data in the model
-- Therefore we sould combine it as type and return this type from the
-- triggerCollectingJobs Tirade
-- by the way renaming the functions from Stefan would also be better...
-- and not to forget - we need a way to map over the list of applications


type alias Model =
    { currentDate : Maybe Date
    , jobs : List Job
    , err : String
    }


triggerCollectingJobs : Cmd Msg
triggerCollectingJobs =
    let
        _ =
            Debug.log "triggerCollectingJobs" "###"
    in
        Task.map2 (,) (loadApplication "datev") Date.now
            |> Task.andThen loadJob
            |> Task.attempt DataLoaded


loadApplication : String -> Task.Task Http.Error JobApplication
loadApplication application =
    let
        _ =
            Debug.log "loadApplication" application
    in
        applicationDecoder
            |> Http.get ("data/application/" ++ application ++ ".json")
            |> Http.toTask


dummyJob =
    Job (createDate "1/1/2004") (createDate "12/31/2008") "Navigon AG" "Software-Engineer" [ "Spracherkennung", "Oberflächenprogrammierung mit C++" ]


loadJob : ( JobApplication, Date.Date ) -> Task.Task Http.Error Job
loadJob ( application, date ) =
    let
        -- proof that job is loaded from the applications list
        -- jobUrl = case (((List.drop 1)>> List.head) application.jobLinks) of
        jobUrl =
            case (List.head application.jobLinks) of
                Just url ->
                    url

                Nothing ->
                    ""

        _ =
            Debug.log "loadJob" (toString application) ++ (toString date)
    in
        createJobDecoder date
            |> Http.get jobUrl
            |> Http.toTask


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model
                Nothing
                []
                ""

        _ =
            Debug.log "INIT" "nothing"
    in
        ( model, batch [ now, triggerCollectingJobs ] )


now : Cmd Msg
now =
    Task.perform (Just >> SetDate) Date.now


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "update msg: " <| (toString msg)
    in
        case msg of
            DataLoaded (Ok job) ->
                ( { model | jobs = List.append model.jobs [ job ] }, Cmd.none )

            DataLoaded (Err error) ->
                ( { model | err = (toString error) }, Cmd.none )

            JobMsg msg ->
                ( model, Cmd.none )

            SetDate maybeDate ->
                ( { model | currentDate = maybeDate }, Cmd.none )


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , case model.currentDate of
            Just date ->
                Html.map JobMsg (div [] (List.map (jobDescription date) model.jobs))

            Nothing ->
                text ""
        , text <| toString model.currentDate
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
