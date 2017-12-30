module JobApplication exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col


type alias JobApplication = 
    { letterParagraphs: List String
    , jobLinks: List String
    , educationLinks: List String
    , miscLinks: List String
    }

applicationDecoder: Decode.Decoder JobApplication
applicationDecoder = decode JobApplication
    |> required "letterParagraphs" (Decode.list Decode.string)
    |> required "jobLinks" (Decode.list Decode.string)
    |> required "educationLinks" (Decode.list Decode.string)
    |> required "miscLinks" (Decode.list Decode.string)

type Msg = Init

applicationView: JobApplication -> Html Msg
applicationView application =
    let
        marginBottom1 = Col.attrs [style [("margin-bottom", "1em")]]
        textRight = Col.attrs [style [("text-align", "right")]]
        defaultCol txt = Grid.col
            [ Col.xs12
            , marginBottom1
            ]
            [ text txt ]
    in
        Grid.row []
            (List.append
                (List.map defaultCol application.letterParagraphs)
                [ Grid.col [ Col.xs12, textRight ]
                    [ text "Mit freundlichen Grüßen" ]
                , Grid.col [ Col.xs12, textRight ]
                    [ text "Christian Hörauf" ]
                ]
            )