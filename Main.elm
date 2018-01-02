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
import Misc exposing (..)
import Platform.Cmd exposing (..)
import Navigation
import UrlParser as Url exposing ((</>), s, string )
-- main : Program Never Model Msg
main =
    Navigation.program UrlChange
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
    | MiscLoaded (Result Http.Error (List Misc))
    | AppLoaded (Result Http.Error JobApplication)
    | SetDate (Maybe Date)
    | AppMsg JobApplication.Msg
    | JobMsg JobDescription.Msg
    | EduMsg Education.Msg
    | MiscMsg Misc.Msg
    | UrlChange Navigation.Location


type alias Model =
    { currentDate : Maybe Date
    , app : JobApplication
    , jobs : List Job
    , edus : List Education
    , misc : List Misc
    , err : String
    }

triggerCollectingDataFromApplication msg getter specificDecoder appUrl =
    let
        _ = Debug.log "triggerCollectingDataFromApplication" appUrl
    in
        loadData applicationDecoder appUrl
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


getApplication: Navigation.Location -> String
getApplication location =
    String.dropLeft 1 location.search

init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        appUrl = "data/application/"++ getApplication location ++".json"

        model =
            Model
                Nothing
                ( JobApplication
                    ["Herzlich Willkommen auf meiner Homepage"]
                    []
                    []
                    []
                )
                []
                []
                []
                ""

        _ =
            Debug.log "INIT" "nothing"
    in
        ( model, batch
            [ now
            , ( Task.attempt AppLoaded ) <| (loadData applicationDecoder appUrl)
            , ( triggerCollectingDataFromApplication EdusLoaded .educationLinks educationDecoder appUrl )
            , ( triggerCollectingDataFromApplication JobsLoaded .jobLinks jobDecoder appUrl )
            , ( triggerCollectingDataFromApplication MiscLoaded .miscLinks miscDecoder appUrl )
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
            AppLoaded (Ok application) ->
                ( {model | app = application }, Cmd.none )

            AppLoaded (Err errors) ->
                ( { model | err = (toString errors) }, Cmd.none )

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

            MiscLoaded (Ok misc) ->
                ( { model | misc = misc}, Cmd.none )

            MiscLoaded (Err errors) ->
                ( { model | err = (toString errors)}, Cmd.none )

            AppMsg msg ->
                ( model, Cmd.none )

            JobMsg msg ->
                ( model, Cmd.none )

            EduMsg msg ->
                ( model, Cmd.none )

            MiscMsg msg ->
                ( model, Cmd.none )
                
            SetDate maybeDate ->
                ( { model | currentDate = maybeDate }, Cmd.none )

            UrlChange location ->
                (model, Cmd.none)


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Html.map AppMsg (div [] [ applicationView model.app ])
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
        , case model.currentDate of
            Just date ->
                Html.map MiscMsg (div [] (List.map (miscView date) model.misc))

            Nothing ->
                text ""
        , text <| toString model.currentDate
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
