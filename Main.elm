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
    = JobsLoaded (Result Http.Error (List Job))
    | SetDate (Maybe Date)
    | JobMsg JobDescription.Msg



-- Therefore we sould combine it as type and return this type from the
-- triggerCollectingJobs Tirade


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
            |> Task.andThen createJobTasks
            -- |> Task.sequence
            |> Task.attempt JobsLoaded


createJobTasks : ( JobApplication, Date ) -> (Task.Task Http.Error (List Job))
createJobTasks ( application, date ) =
    Task.sequence ( List.map (loadJob date) application.jobLinks )


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


loadJob : Date.Date -> String -> Task.Task Http.Error Job
loadJob date jobUrl =
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
            JobsLoaded (Ok jobs) ->
                ( { model | jobs = jobs }, Cmd.none )

            JobsLoaded (Err errors) ->
                ( { model | err = (toString errors) }, Cmd.none )
                
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
