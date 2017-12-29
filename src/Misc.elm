module Misc exposing (..)

import Timespan exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)
import Date exposing (..)

import Html exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
-- import List.FlatMap exposing (flatMap)

type alias LiveSection =
    { timespans : List Timespan
    , descriptions : List String
    }

type alias Misc =
    { title : String
    , liveSections : List LiveSection
    }


join : List (List a) -> List a
join =
  List.foldr (++) []

flatMap : (a -> List b) -> List a -> List b
flatMap f list =
  List.map f list
    |> join

zip : List a -> List b -> List (a,b)
zip xs ys = 
    case (xs, ys) of
        (x :: xTail, y :: yTail) ->
        (x, y) :: zip xTail yTail
        (_, _) ->
        []


timespanDecoder : Decode.Decoder Timespan
timespanDecoder = 
    let
        date : Decode.Decoder Date
        date =
            let
                convert : String -> Decode.Decoder Date
                convert raw =
                    case Date.fromString raw of
                        Ok date ->
                            Decode.succeed date

                        Err error ->
                            Decode.fail error
            in
                Decode.string |> Decode.andThen convert
    in
        decode Timespan
            |> required "start" date
            |> required "end" (Decode.maybe date)



liveSectionDecoder : Decode.Decoder LiveSection
liveSectionDecoder = decode LiveSection
    |> required "timespans" (Decode.list timespanDecoder)
    |> required "descriptions" (Decode.list Decode.string) 

miscDecoder : Decode.Decoder Misc
miscDecoder = decode Misc
    |> required "title" Decode.string
    |> required "liveSections" (Decode.list liveSectionDecoder)

miscView : Date -> Misc -> Html Msg
miscView currentDate misc =
    let
        renderCol colWidth txt = 
            Grid.col
            [ Col.xs12, colWidth ]
            [ text txt ]

        renderLiveSection liveSection =
            let
                timespansAsStrings = List.map (renderTimespan currentDate) liveSection.timespans
                maxListLength = max (List.length timespansAsStrings) (List.length liveSection.descriptions)
                normList list = 
                    if (List.length list) < maxListLength then
                        List.append 
                            list 
                            ((List.map (\_ -> "")) 
                                <| (List.range 1) 
                                <| ((-) maxListLength) (List.length list))
                    else 
                        list
            in
                
                flatMap
                    (\(timespan, description) -> 
                        List.append [(renderCol Col.md3 timespan)] [(renderCol Col.md9 description)]
                    )   
                    (zip (normList timespansAsStrings) (normList liveSection.descriptions))
    in
        Grid.row []
            ( List.append
                [ Grid.col
                    [ Col.xs12 ]
                    [ h3 [] 
                        [ text misc.title
                        ]
                    ]
                ]
                (flatMap renderLiveSection misc.liveSections)
            )

type Msg = Init