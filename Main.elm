module Main exposing (main)

import Types exposing (..)
import Html exposing (Html, div, p, text, h1)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Dict exposing (Dict)
import Time exposing (every, second)
import Task


{-
   World is 64 x 128 or 8192 cells, contains 3000 frogs and 3000 turtles
-}


main : Program Never Model Msg
main =
    Html.program
        { init = init worldWidth worldHeight
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


worldWidth : Int
worldWidth =
    64


worldHeight : Int
worldHeight =
    128


numPads : Int
numPads =
    worldWidth * worldHeight



-- half frogs and half turtles


numCritters : Int
numCritters =
    6000



--type Seed


emptyWorld : Int -> Int -> Model
emptyWorld width height =
    { width = width
    , height = height
    , world = Dict.empty
    }


makePad : Int -> Int -> Critter -> { critter : Critter, location : Point, neighboors : List Point }
makePad x y critter =
    { location = ( x, y )
    , critter = critter
    , neighboors = makeNeighboors x y
    }



{- like (<*>) in Haskell, specialised to lists -}


andMap : List (a -> b) -> List a -> List b
andMap listOfFuncs listOfAs =
    List.concatMap (\f -> List.map f listOfAs) listOfFuncs



{- generate List of (x,y) for every point in x1 to x2 by y1 to y2 -}


grid : Int -> Int -> Int -> Int -> List ( Int, Int )
grid x1 x2 y1 y2 =
    andMap (List.map (,) (List.range x1 x2)) (List.range y1 y2)



{- up to 8 neighboors and on the map -}


makeNeighboors : Int -> Int -> List Point
makeNeighboors x y =
    let
        isOnMap x y =
            x >= 0 && x < worldWidth && y >= 0 && y < worldHeight
    in
        grid (x - 1) (x + 1) (y - 1) (y + 1)
            |> List.filter (\( x1, y1 ) -> ( x1, y1 ) /= ( x, y ) && isOnMap x1 y1)


init : Int -> Int -> ( Model, Cmd Msg )
init width height =
    ( emptyWorld width height, getTime )


getTime : Cmd Msg
getTime =
    Task.perform OnTime Time.now


seedWorld : Int -> Model -> Model
seedWorld seed model =
    let
        randomPoints =
            (randomListOfPoints seed model.width model.height numCritters)

        randomPointsWithIndex =
            List.map2 (,) (List.range 1 numCritters) randomPoints

        newWorld =
            List.foldl
                (\( i, ( x, y ) ) d ->
                    let
                        critter =
                            if i >= (numCritters // 2) then
                                Frog
                            else
                                Turtle
                    in
                        d |> Dict.insert ( x, y ) (makePad x y critter)
                )
                Dict.empty
                randomPointsWithIndex
    in
        { model | world = newWorld }


randomListOfPoints : Int -> Int -> Int -> Int -> List Point
randomListOfPoints seed width height len =
    let
        initialSeed =
            Random.initialSeed seed

        --generatorPair =
        --    Random.pair (Random.int 0 (width - 1)) (Random.int 0 (height - 1))
        --
        generatorList =
            Random.list len <| Random.pair (Random.int 0 (width - 1)) (Random.int 0 (height - 1))

        ( points, newSeed ) =
            Random.step generatorList initialSeed
    in
        points


viewCell : Model -> Int -> Int -> Html msg
viewCell model row col =
    let
        determineColor pad =
            case pad.critter of
                Frog ->
                    "green"

                Turtle ->
                    "brown"

                _ ->
                    "black"

        color =
            Dict.get ( col, row ) model.world
                |> Maybe.map determineColor
                |> Maybe.withDefault "black"
    in
        div
            [ style
                [ ( "width", "2px" )
                , ( "height", "2px" )
                , ( "background-color", color )
                , ( "display", "inline-block" )
                , ( "border", "none" )
                , ( "vertical-align", "top" )
                ]
            ]
            []


viewRow : Model -> Int -> Html msg
viewRow model row =
    div
        [ style
            [ ( "height", "2px" ) ]
        ]
        (List.map (viewCell model row) <| List.range 0 model.width)


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "Pond"
            ]
        , div
            [ style
                [ ( "padding", "8px" ) ]
            ]
            (List.map (viewRow model) <| List.range 0 model.height)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTime t ->
            ( seedWorld (round t) model, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : model -> Sub msg
subscriptions model =
    Sub.none
