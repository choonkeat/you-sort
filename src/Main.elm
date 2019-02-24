module Main exposing (main)

import Browser
import Browser.Navigation
import Data
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, h1, h2, iframe, input, li, main_, node, p, pre, progress, text, ul)
import Html.Attributes exposing (attribute, class, href, placeholder, rel, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import LocalStorage
import Random
import Random.List exposing (shuffle)
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


type alias Model =
    { navKey : Browser.Navigation.Key
    , sorted : List Data.Subject
    , progress : List (List Data.Subject)
    , unsorted : List Data.Subject
    , notes : Dict Data.Subject String
    }


type alias Flags =
    {}


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | EnteredNotes Data.Subject String
    | Greater Data.Subject
    | Lesser Data.Subject
    | Shuffled (List Data.Subject)
    | StoreChanged LocalStorage.Event
    | ClearStore


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            { navKey = navKey
            , sorted = []
            , progress = []
            , unsorted = []
            , notes = Dict.fromList []
            }

        cmd =
            if model.progress == [] then
                LocalStorage.request "you-sort"

            else
                saveState model
    in
    ( model, cmd )


view : Model -> Browser.Document Msg
view model =
    Browser.Document "App"
        [ node "link"
            [ rel "stylesheet"
            , href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
            ]
            []
        , main_ [ class "container" ]
            [ div [ class "containers" ]
                [ viewPair model
                , debugView model
                , button [ class "btn btn-danger", onClick ClearStore ] [ text "Reset" ]
                ]
            ]
        ]


debugView : Model -> Html Msg
debugView model =
    let
        allNotes =
            Dict.foldl
                (\id str acc ->
                    ( id, str ) :: acc
                )
                []
                model.notes
    in
    div []
        [ pre [ style "white-space" "pre-wrap" ]
            [ text "model =\n"
            , text "    { navKey   = navKey\n"
            , text "    , sorted   = "
            , text (Debug.toString model.sorted)
            , text "\n"
            , text "    , progress = "
            , text (Debug.toString model.progress)
            , text "\n"
            , text "    , unsorted = "
            , text (Debug.toString model.unsorted)
            , text "\n"
            , text "    , notes    = Dict.fromList "
            , text (Debug.toString allNotes)
            , text "\n"
            , text "    }"
            ]
        ]


viewPair : Model -> Html Msg
viewPair model =
    case ( model.unsorted, model.progress ) of
        ( u1 :: us, greaters :: [ pivot ] :: lessers :: others ) ->
            div
                [ class "row mt-2 mb-2"
                , style "border" "1px solid black"
                ]
                (List.append
                    [ viewOne model u1 (Greater u1)
                    , viewOne model pivot (Lesser u1)
                    , div [ class "w-100" ] []
                    ]
                    []
                )

        _ ->
            text ""


viewOne : Model -> Data.Subject -> Msg -> Html Msg
viewOne model subject msg =
    div [ class "col mb-2" ]
        [ Data.viewSubject subject
        , div [ class "form-group" ]
            [ input
                [ type_ "text"
                , class "form-control"
                , placeholder "Optional notes for yourself..."
                , onInput (EnteredNotes subject)
                , value (Maybe.withDefault "" <| Dict.get subject model.notes)
                ]
                []
            ]
        , button
            [ class "btn btn-primary btn-block"
            , onClick msg
            ]
            [ text "Choose" ]

        -- present the information somehow, image-by-id? etc
        -- for the user to judge and choose the "better" one
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest urlRequestBrowser ->
            ( model, Cmd.none )

        OnUrlChange urlUrl ->
            ( model, Cmd.none )

        Shuffled dataset ->
            let
                newprogress =
                    [ [], List.take 1 dataset, [] ]

                newunsorted =
                    List.drop 1 dataset

                newmodel =
                    { model | progress = newprogress, unsorted = newunsorted, sorted = [] }
            in
            ( model, saveState newmodel )

        EnteredNotes subject string ->
            let
                newnotes =
                    Dict.insert subject string model.notes

                newmodel =
                    { model | notes = newnotes }
            in
            ( model, saveState newmodel )

        Greater subject ->
            let
                newprogress =
                    case model.progress of
                        greaters :: pivots :: lessers :: others ->
                            (subject :: greaters) :: pivots :: lessers :: others

                        _ ->
                            Debug.log "cannot greater" model.progress

                newmodel =
                    updateNewProgress model newprogress
            in
            ( model, saveState newmodel )

        Lesser subject ->
            let
                newprogress =
                    case model.progress of
                        greaters :: pivots :: lessers :: others ->
                            greaters :: pivots :: (subject :: lessers) :: others

                        _ ->
                            Debug.log "cannot lesser" model.progress

                newmodel =
                    updateNewProgress model newprogress
            in
            ( model, saveState newmodel )

        ClearStore ->
            ( model, Data.newRandomState Shuffled )

        StoreChanged v ->
            case v of
                LocalStorage.Updated key maybeValue ->
                    case maybeValue of
                        Nothing ->
                            ( model, Data.newRandomState Shuffled )

                        Just s ->
                            case Json.Decode.decodeString (decodeModel model) s of
                                Err err ->
                                    let
                                        _ =
                                            Debug.log "localStorage err" err
                                    in
                                    ( model, Cmd.none )

                                Ok newmodel ->
                                    ( newmodel, Cmd.none )

                LocalStorage.WriteFailure key maybeValue err ->
                    let
                        _ =
                            Debug.log "localStorage fail" v
                    in
                    ( model, Cmd.none )

                LocalStorage.BadMessage err ->
                    let
                        _ =
                            Debug.log "localStorage bad" v
                    in
                    ( model, Cmd.none )


updateNewProgress : Model -> List (List Data.Subject) -> Model
updateNewProgress model newprogress =
    case model.unsorted of
        t1 :: t2 :: ts ->
            { model | progress = newprogress, unsorted = t2 :: ts }

        _ ->
            case newprogress of
                (g1 :: g2 :: gs) :: others ->
                    { model | progress = List.append [ [], [ g1 ], [] ] others, unsorted = g2 :: gs }

                [ g1 ] :: others ->
                    let
                        newmodel =
                            { model | sorted = List.append model.sorted [ g1 ] }
                    in
                    updateNewProgress newmodel others

                [] :: others ->
                    updateNewProgress model others

                [] ->
                    { model | progress = newprogress, unsorted = [] }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map StoreChanged LocalStorage.watchChanges



-- SAVE STATE


saveState : Model -> Cmd Msg
saveState model =
    Json.Encode.encode 2 (encodeModel model)
        |> LocalStorage.save "you-sort"


encodeNote : ( Data.Subject, String ) -> Json.Encode.Value
encodeNote v =
    let
        ( v0, v1 ) =
            v
    in
    Json.Encode.list Json.Encode.string [ Data.subjectString v0, v1 ]


encodeNotesList : List ( Data.Subject, String ) -> Json.Encode.Value
encodeNotesList =
    Json.Encode.list encodeNote


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    Json.Encode.object
        [ ( "sorted", Json.Encode.list Data.subjectEncoder model.sorted )
        , ( "progress", Json.Encode.list (Json.Encode.list Data.subjectEncoder) model.progress )
        , ( "unsorted", Json.Encode.list Data.subjectEncoder model.unsorted )
        , ( "notes", encodeNotesList (Dict.toList model.notes) )
        ]


decodeNotesList : Json.Decode.Decoder (List ( Data.Subject, String ))
decodeNotesList =
    Json.Decode.list
        (Json.Decode.map2 Tuple.pair
            (Json.Decode.index 0 Data.subjectDecoder)
            (Json.Decode.index 1 Json.Decode.string)
        )


decodeModel : Model -> Json.Decode.Decoder Model
decodeModel model =
    Json.Decode.map5 Model
        (Json.Decode.succeed model.navKey)
        (Json.Decode.field "sorted" (Json.Decode.list Data.subjectDecoder))
        (Json.Decode.field "progress" (Json.Decode.list (Json.Decode.list Data.subjectDecoder)))
        (Json.Decode.field "unsorted" (Json.Decode.list Data.subjectDecoder))
        (Json.Decode.field "notes" (decodeNotesList |> Json.Decode.map Dict.fromList))
