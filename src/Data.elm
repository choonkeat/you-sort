module Data exposing (Subject, newRandomState, subjectDecoder, subjectEncoder, subjectString, viewSubject)

import Html exposing (Html, h1, text)
import Json.Decode
import Json.Encode
import Random
import Random.List exposing (shuffle)


type alias Subject =
    Int


newRandomState : (List Subject -> msg) -> Cmd msg
newRandomState msg =
    shuffle (List.range 1 10)
        |> Random.generate msg


viewSubject : Subject -> Html msg
viewSubject n =
    h1 [] [ text (subjectString n) ]


subjectEncoder : Subject -> Json.Encode.Value
subjectEncoder =
    Json.Encode.int


subjectDecoder : Json.Decode.Decoder Subject
subjectDecoder =
    Json.Decode.int


subjectString : Subject -> String
subjectString =
    String.fromInt
