module JobDescription exposing (..)

import Types exposing (..)

import Html exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

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