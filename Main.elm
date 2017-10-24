module Main exposing (..)

import Html exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Date exposing (..)
import List


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Job =
    { start : Date
    , end : Date
    , employer : String
    , title : String
    , tasks : List String
    }


type alias Model =
    { jobs : Job
    }

createDate: String -> Date
createDate str =
    fromString str |> Result.withDefault (Date.fromTime 0)

init : ( Model, Cmd Msg )
init =
    ( Model (Job (createDate "1.1.2004") (createDate "31.12.2008") "Navigon AG" "Software-Engineer" ["Spracherkennung", "Oberflächenprogrammierung mit C++"]), Cmd.none )


type Msg
    = Msg1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        -- , Grid.row []
        --     [ Grid.col []
        --         [ text (model.jobs.title ++ " bei " ++ model.jobs.employer) ]
        --     ]
        , jobDescription model.jobs
        ]



jobDescription : Job -> Html Msg
jobDescription job =
    let 
        entry:String -> Html Msg
        entry task = li [] [text task]

        
    in
        Grid.row []
            [ Grid.col 
                [ Col.md9, Col.pushMd3 ]
                [ text (job.title ++ " bei " ++ job.employer)
                , ul [] (List.map entry job.tasks)
                ]
            , Grid.col
                [Col.md3, Col.pullMd9]
                [text "zeit"]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
