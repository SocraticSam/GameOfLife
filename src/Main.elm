module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Random
import Time


dimension : Int
dimension =
    150


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = always init
        , subscriptions = subscriptions
        }


type alias Model =
    Grid


type alias Grid =
    Array Cell


type Cell
    = Alive
    | Dead


init : ( Model, Cmd Msg )
init =
    let
        cellGenerator : Random.Generator Cell
        cellGenerator =
            Random.int 0 1
                |> Random.map
                    (\int ->
                        if int == 1 then
                            Alive

                        else
                            Dead
                    )
    in
    ( Array.initialize (dimension * dimension) (always Dead)
    , Random.list (dimension * dimension) cellGenerator
        |> Random.map Array.fromList
        |> Random.generate Initialize
    )


type Msg
    = Initialize Grid
    | Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialize grid ->
            ( grid, Cmd.none )

        Tick ->
            ( tick model, Cmd.none )


tick : Model -> Model
tick g =
    g
        |> Array.indexedMap
            (\index cell ->
                let
                    neighbours =
                        getNeighbours index g

                    aliveNeighbours =
                        List.filter ((==) Alive) neighbours

                    aliveCount =
                        List.length aliveNeighbours
                in
                case ( cell, aliveCount ) of
                    ( _, 3 ) ->
                        Alive

                    ( Alive, 2 ) ->
                        Alive

                    _ ->
                        Dead
            )


getNeighbours : Int -> Model -> List Cell
getNeighbours index g =
    let
        currentCoords =
            indexToCoords index

        upperLeft =
            currentCoords
                |> Maybe.map
                    (Tuple.mapBoth (\x -> x - 1) (\y -> y - 1))

        upperCenter =
            currentCoords
                |> Maybe.map
                    (Tuple.mapSecond (\y -> y - 1))

        upperRight =
            currentCoords
                |> Maybe.map
                    (Tuple.mapBoth (\x -> x + 1) (\y -> y - 1))

        left =
            currentCoords
                |> Maybe.map
                    (Tuple.mapFirst (\x -> x - 1))

        right =
            currentCoords
                |> Maybe.map
                    (Tuple.mapFirst (\x -> x + 1))

        lowerLeft =
            currentCoords
                |> Maybe.map
                    (Tuple.mapBoth (\x -> x - 1) (\y -> y + 1))

        lowerCenter =
            currentCoords
                |> Maybe.map
                    (Tuple.mapSecond (\y -> y + 1))

        lowerRight =
            currentCoords
                |> Maybe.map
                    (Tuple.mapBoth (\x -> x + 1) (\y -> y + 1))
    in
    [ upperLeft, upperCenter, upperRight, left, right, lowerLeft, lowerCenter, lowerRight ]
        |> List.map (Maybe.andThen coordsToIndex)
        |> List.map (Maybe.andThen (lookupInGrid g))
        |> List.filterMap identity


lookupInGrid : Model -> Int -> Maybe Cell
lookupInGrid g index =
    Array.get index g


coordsToIndex : ( Int, Int ) -> Maybe Int
coordsToIndex ( x, y ) =
    if x < 0 || y < 0 || x >= dimension || y >= dimension then
        Nothing

    else
        Just (y * dimension + x)


indexToCoords : Int -> Maybe ( Int, Int )
indexToCoords index =
    let
        y =
            index
                // dimension

        x =
            modBy dimension index
    in
    if index < 0 || index >= dimension * dimension then
        Nothing

    else
        Just ( x, y )


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "display" "grid"
        , Html.Attributes.style "grid-template-columns"
            (List.repeat (floor (sqrt (toFloat (Array.length model))))
                (((1 / sqrt (toFloat (Array.length model)) * 100) |> String.fromFloat) ++ "%")
                |> String.join " "
            )
        , Html.Attributes.style "grid-gap" "1px"
        , Html.Attributes.style "background-color" "#aaa"
        , Html.Attributes.style "width" "99vw"
        , Html.Attributes.style "height" "98vh"
        ]
        (model
            |> Array.indexedMap
                (\index value ->
                    Html.div
                        [ Html.Attributes.style "color" "green"
                        , if value == Alive then
                            Html.Attributes.style "background-color" "#222"

                          else
                            Html.Attributes.style "background-color" "#ddd"
                        ]
                        []
                )
            |> Array.toList
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Browser.Events.onKeyPress (Decode.succeed Tick)
    Time.every 1000 (always Tick)
