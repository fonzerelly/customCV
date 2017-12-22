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
import Education exposing (..)
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
    | EdusLoaded (Result Http.Error (List Education))
    | SetDate (Maybe Date)
    | JobMsg JobDescription.Msg
    | EduMsg Education.Msg


type alias Model =
    { currentDate : Maybe Date
    , jobs : List Job
    , edus : List Education
    , err : String
    }

triggerCollectingDataFromApplication msg getter specificDecoder =
    let
        _ = Debug.log "triggerCollectingDataFromApplication" "###"
    in
        loadData applicationDecoder "data/application/datev.json"
            |> Task.andThen (createDataTasks getter specificDecoder)
            |> Task.attempt msg

createDataTasks : (b -> List String) -> Decode.Decoder a -> b -> Task.Task Error (List a)
createDataTasks getter specificDecoder application =
    let
        _ = Debug.log "createDataTasks" (toString application)
    in
        Task.sequence ( List.map (loadData specificDecoder) (getter application) )


loadData : Decode.Decoder a -> String -> Task.Task Error a
loadData specificDecoder url =
    specificDecoder
        |> Http.get url
        |> Http.toTask

init : ( Model, Cmd Msg )
init =
    let
        model =
            Model
                Nothing
                []
                []
                ""

        _ =
            Debug.log "INIT" "nothing"
    in
        ( model, batch
            [ now
            , ( triggerCollectingDataFromApplication EdusLoaded .educationLinks educationDecoder )
            , ( triggerCollectingDataFromApplication JobsLoaded .jobLinks jobDecoder )
            ]
        )


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

            EdusLoaded (Ok edus) ->
                let
                    _ = Debug.log "update" (toString edus)
                in
                    ( { model | edus = edus }, Cmd.none )

            EdusLoaded (Err errors) ->
                ( { model | err = (toString errors) }, Cmd.none )

            JobMsg msg ->
                ( model, Cmd.none )

            EduMsg msg ->
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
        , case model.currentDate of
            Just date ->
                Html.map EduMsg (div [] (List.map (educationView date) model.edus))

            Nothing ->
                text ""
        , text <| toString model.currentDate
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
