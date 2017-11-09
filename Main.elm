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
import Platform.Cmd exposing (..)

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
    = DataLoaded (Result Http.Error Job)
    | SetDate (Maybe Date)
    | JobMsg JobDescription.Msg


type alias Model =
    { currentDate: Maybe Date
    , jobs : Job
    , err: String
    }

decodeData : Decode.Decoder String
decodeData = Decode.string

fetchData : Date -> Cmd Msg
fetchData currentDate =
    let
        decoder = createJobDecoder currentDate
        request: Http.Request Job 
        request = Http.get "data/curriculum-vitae.json" decoder
    in
        Http.send DataLoaded request


dummyJob = Job (createDate "1/1/2004") (createDate "12/31/2008") "Navigon AG" "Software-Engineer" ["Spracherkennung", "Oberflächenprogrammierung mit C++"]

init : ( Model, Cmd Msg )
init =
    let
        model =  Model
            Nothing
            dummyJob
            ""

    in
        (model, now)

now : Cmd Msg
now = Task.perform (Just >> SetDate) Date.now

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataLoaded (Ok job) ->
            ( {model| jobs = job}, Cmd.none )

        DataLoaded (Err error) ->
            ( {model| err = (toString error)}, Cmd.none )

        JobMsg msg ->
            ( model, Cmd.none )

        SetDate maybeDate ->
            case maybeDate of
                Just date -> 
                    ( {model| currentDate = maybeDate}, fetchData date)
                Nothing ->
                    (model, Cmd.none)

view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , case model.currentDate of
            Just date -> Html.map JobMsg (jobDescription date model.jobs)
            Nothing -> text ""
        , text <| toString model.currentDate
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
