module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Bg
import Element.Events as Events
import Html exposing (Html)
import Random


type Cell
    = Red
    | Green
    | Blue
    | Orange


type alias Row =
    List (Maybe Cell)


type alias Stage =
    List Row


type alias Model =
    Stage


type Msg
    = Initialized Stage
    | Clicked (Maybe Cell) { x : Int, y : Int }


init : ( Model, Cmd Msg )
init =
    let
        initStage =
            Random.uniform Red [ Blue, Green, Orange ]
                |> Random.list 6
                |> Random.map (List.map Just)
                |> Random.list 25
                |> Random.generate Initialized
    in
    ( [], initStage )


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialized stage ->
            ( stage, Cmd.none )

        Clicked cell pos ->
            let
                stage =
                    model
                        |> eraseAround cell pos
                        |> sweep
            in
            ( stage, Cmd.none )


surroundings : { x : Int, y : Int } -> List { x : Int, y : Int }
surroundings pos =
    [ { pos | x = pos.x - 1 }
    , { pos | x = pos.x + 1 }
    , { pos | y = pos.y - 1 }
    , { pos | y = pos.y + 1 }
    ]
        |> List.filter (\{ x } -> 0 <= x && x < 25)
        |> List.filter (\{ y } -> 0 <= y && y < 6)


getAt : { x : Int, y : Int } -> Stage -> Maybe Cell
getAt { x, y } stage =
    let
        get i =
            List.drop i >> List.head
    in
    stage
        |> get x
        |> Maybe.andThen (get y)
        |> Maybe.andThen identity


eraseAt : { x : Int, y : Int } -> Stage -> Stage
eraseAt { x, y } stage =
    let
        doAt i fn xs =
            case ( i, xs ) of
                ( _, [] ) ->
                    []

                ( 0, e :: es ) ->
                    fn e :: es

                ( _, e :: es ) ->
                    e :: doAt (i - 1) fn es

        eraseNth i xs =
            doAt i (always Nothing) xs
    in
    doAt x (eraseNth y) stage


eraseAround : Maybe Cell -> { x : Int, y : Int } -> Stage -> Stage
eraseAround target self stage =
    if getAt self stage == target then
        eraseAt self stage
            |> (\cs -> List.foldl (eraseAround target) cs (surroundings self))

    else
        stage


sweepV : Stage -> Stage
sweepV =
    let
        comp a _ =
            if a == Nothing then
                LT

            else
                EQ
    in
    List.map (List.sortWith comp)


sweepH : Stage -> Stage
sweepH =
    List.filter (List.any ((/=) Nothing))


sweep : Stage -> Stage
sweep =
    sweepV >> sweepH


view : Model -> Html Msg
view model =
    layout [ padding 8 ] (viewStage model)


viewStage : Model -> Element Msg
viewStage model =
    row [ spacing 2 ] (List.indexedMap viewRow model)


viewRow : Int -> List (Maybe Cell) -> Element Msg
viewRow i cs =
    column [ spacing 2 ] (List.indexedMap (viewCell i) cs)


viewCell : Int -> Int -> Maybe Cell -> Element Msg
viewCell x y cell =
    let
        onClick =
            Events.onClick (Clicked cell { x = x, y = y })

        filler =
            el [ width (px 20), height (px 20) ] none
    in
    case cell of
        Just Red ->
            el [ Bg.color (rgb 1 0.2 0.3), onClick ] filler

        Just Green ->
            el [ Bg.color (rgb 0.2 1 0.3), onClick ] filler

        Just Blue ->
            el [ Bg.color (rgb 0.2 0.3 1), onClick ] filler

        Just Orange ->
            el [ Bg.color (rgb 1 0.75 0.25), onClick ] filler

        Nothing ->
            filler
