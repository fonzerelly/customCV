module JobApplication exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (..)


type alias JobApplication = 
    { jobLinks: List String
    , educationLinks: List String
    }

applicationDecoder: Decode.Decoder JobApplication
applicationDecoder = decode JobApplication
    |> required "jobLinks" (Decode.list Decode.string)
    |> required "educationLinks" (Decode.list Decode.string)