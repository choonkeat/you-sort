module Main exposing (main)

import Browser
import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, h1, h2, iframe, input, li, main_, node, p, pre, progress, text, ul)
import Html.Attributes exposing (attribute, class, href, placeholder, rel, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
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
    , sorted : List Subject
    , progress : List (List Subject)
    , unsorted : List Subject
    , notes : Dict Subject String
    }


type alias Subject =
    String


type alias Flags =
    {}


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | EnteredNotes Subject String
    | Greater Subject
    | Lesser Subject
    | Shuffled (List Int)


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
                Random.generate Shuffled (shuffle (List.range 1 10))

            else
                Cmd.none
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
                [ viewSubjects model
                , debugView model
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
        , p [ class "text-muted" ]
            [ text "-- This app does not save state."
            , text " So, copy the `code` above anytime to \"save\""
            , text " and overwrite your `init` to \"restore\"."
            ]
        ]


viewSubjects : Model -> Html Msg
viewSubjects model =
    case ( model.unsorted, model.progress ) of
        ( u1 :: us, greaters :: [ pivot ] :: lessers :: others ) ->
            div
                [ class "row mt-2 mb-2"
                , style "border" "1px solid black"
                ]
                (List.append
                    [ viewSubject model u1 (Greater u1)
                    , viewSubject model pivot (Lesser u1)
                    , div [ class "w-100" ] []
                    ]
                    []
                )

        _ ->
            text ""


viewSubject : Model -> Subject -> Msg -> Html Msg
viewSubject model subject msg =
    div [ class "col mb-2" ]
        [ h1 [] [ text subject ]
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

        Shuffled numberlist ->
            let
                dataset =
                    List.map String.fromInt numberlist

                newprogress =
                    [ [], List.take 1 dataset, [] ]

                newunsorted =
                    List.drop 1 dataset
            in
            ( { model | progress = newprogress, unsorted = newunsorted }, Cmd.none )

        EnteredNotes subject string ->
            let
                newnotes =
                    Dict.insert subject string model.notes
            in
            ( { model | notes = newnotes }, Cmd.none )

        Greater subject ->
            let
                newprogress =
                    case model.progress of
                        greaters :: pivots :: lessers :: others ->
                            (subject :: greaters) :: pivots :: lessers :: others

                        _ ->
                            Debug.log "cannot greater" model.progress
            in
            ( updateNewProgress model newprogress, Cmd.none )

        Lesser subject ->
            let
                newprogress =
                    case model.progress of
                        greaters :: pivots :: lessers :: others ->
                            greaters :: pivots :: (subject :: lessers) :: others

                        _ ->
                            Debug.log "cannot lesser" model.progress
            in
            ( updateNewProgress model newprogress, Cmd.none )


updateNewProgress : Model -> List (List Subject) -> Model
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
    Sub.none
