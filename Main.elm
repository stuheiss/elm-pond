module Main exposing (main)

import Types exposing (..)
import Html exposing (Html, div, p, text, h1, img, button)
import Html.Attributes exposing (..)
import Random
import Dict exposing (Dict)
import Time exposing (every, second, inMilliseconds, inSeconds)
import Task
import Random.List
import Date exposing (fromTime)
import Html.Events exposing (onClick)


--import Tuple
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
    , time = 0
    , seed = Random.initialSeed 0
    }


makePad : Int -> Int -> Critter -> { critter : Critter, location : Point, neighboors : List Point }
makePad x y critter =
    let
        neighboors =
            case critter of
                Empty ->
                    []

                _ ->
                    makeNeighboors x y
    in
        { location = ( x, y )
        , critter = critter
        , neighboors = neighboors
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
        -- make enough extra random points to deal with duplicate point pairs
        numPointsWanted =
            2 * numCritters

        ( randomPoints, newSeed ) =
            (randomListOfPoints seed model.width model.height numPointsWanted)

        newWorld =
            List.foldl
                (\( x, y ) d ->
                    let
                        critter =
                            if Dict.size d >= (numCritters // 2) then
                                Frog
                            else
                                Turtle
                    in
                        if Dict.size d < numCritters then
                            d |> Dict.insert ( x, y ) (makePad x y critter)
                        else
                            d
                )
                Dict.empty
                randomPoints
    in
        { model | world = newWorld, seed = newSeed }


randomListOfPoints : Int -> Int -> Int -> Int -> ( List Point, Random.Seed )
randomListOfPoints seed width height len =
    let
        initialSeed =
            Random.initialSeed seed

        generatorPair =
            Random.pair (Random.int 0 (width - 1)) (Random.int 0 (height - 1))

        generatorListOfPoints =
            Random.list len generatorPair
    in
        Random.step generatorListOfPoints initialSeed



-- this blows up with stack overflow
--randomListOfPoints : Int -> Int -> Int -> Int -> ( List Point, Random.Seed )
--randomListOfPoints seed width height len =
--    let
--        initialSeed =
--            Random.initialSeed seed
--        listOfPoints =
--            grid 0 (width - 1) 0 (height - 1)
--        generatorListOfPoints =
--            Random.List.shuffle listOfPoints
--    in
--        Random.step generatorListOfPoints initialSeed
-- VIEW


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

        --determineImgSrc pad =
        --    case pad.critter of
        --        Frog ->
        --            [ img [ src "images/icons8-Frog-48.png", height 10, width 10 ] [] ]
        --        Turtle ->
        --            [ img [ src "images/icons8-Turtle-48.png", height 10, width 10 ] [] ]
        --        _ ->
        --            [ img [ src "images/icons8-blank-48.png", height 10, width 10 ] [] ]
        color =
            Dict.get ( col, row ) model.world
                |> Maybe.map determineColor
                |> Maybe.withDefault "black"

        --imgsrc =
        --    Dict.get ( col, row ) model.world
        --        |> Maybe.map determineImgSrc
        --        |> Maybe.withDefault [ img [ src "images/icons8-blank-48.png", height 10, width 10 ] [] ]
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



--div
--    [ style
--        [ ( "width", "10px" )
--        , ( "height", "10px" )
--        , ( "background-color", "black" )
--        , ( "display", "inline-block" )
--        , ( "border", "none" )
--        , ( "vertical-align", "top" )
--        ]
--    ]
--    imgsrc
--[ class "picture" ]
--[ img [ src "images/icons8-Frog-48.png", height 10, width 10 ] []
--]


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
        , p []
            [ text <| toString <| fromTime <| inMilliseconds model.time
            ]

        --, p []
        --    [ text <| toString <| round <| inSeconds model.time
        --    ]
        , p []
            [ text <| "critters: " ++ (toString <| Dict.size model.world) ]
        , p []
            [ text <| "worldSize: " ++ (toString <| model.width * model.height) ]
        , div
            [ style
                [ ( "padding", "8px" ) ]
            ]
            (List.map (viewRow model) <| List.range 0 model.height)
        , p []
            [ button [ onClick Reset ] [ text "Reset" ]
            ]
        ]



--- MOVE


emptyLilypad : Lilypad
emptyLilypad =
    { location = ( 0, 0 ), critter = Empty, neighboors = [] }


getLilypad : Point -> World -> Lilypad
getLilypad point world =
    Dict.get point world
        |> Maybe.withDefault emptyLilypad


critterWillMove : Point -> World -> Bool
critterWillMove point world =
    let
        thisLilypad =
            getLilypad point world

        neighboorsTotal =
            List.length thisLilypad.neighboors

        neighboorsSameKind =
            List.foldl
                (\point acc ->
                    let
                        anotherLilypad =
                            getLilypad point world
                    in
                        if anotherLilypad.critter == thisLilypad.critter then
                            1 + acc
                        else
                            acc
                )
                0
                thisLilypad.neighboors
    in
        (toFloat neighboorsSameKind / toFloat neighboorsTotal) < 0.3


getCritterMove : Point -> World -> Random.Seed -> ( Maybe ( Critter, Point ), Random.Seed )
getCritterMove point world seed =
    let
        thisLilypad =
            getLilypad point world

        neighboorsTotal =
            List.length thisLilypad.neighboors

        neighboorsSameKind =
            List.foldl
                (\point acc ->
                    let
                        anotherLilypad =
                            getLilypad point world
                    in
                        if anotherLilypad.critter == thisLilypad.critter then
                            1 + acc
                        else
                            acc
                )
                0
                thisLilypad.neighboors

        getCritterNeighboors point =
            let
                thisLilypad =
                    getLilypad point world
            in
                List.foldl
                    (\anotherPoint acc ->
                        let
                            anotherLilypad =
                                getLilypad anotherPoint world
                        in
                            ( anotherLilypad.critter, anotherPoint ) :: acc
                    )
                    []
                    thisLilypad.neighboors

        getEmptyNeighboors point =
            List.filter
                (\( critter, anotherPoint ) -> critter == Empty)
            <|
                getCritterNeighboors point

        getRandomEmptyNeighboor point =
            let
                ( result, newSeed ) =
                    Random.step (Random.List.shuffle (getEmptyNeighboors point)) seed

                newPoint =
                    List.head result
            in
                ( newPoint, newSeed )
    in
        if (toFloat neighboorsSameKind / toFloat neighboorsTotal) < 0.3 then
            getRandomEmptyNeighboor point
        else
            ( Nothing, seed )


moveOneCritter : Point -> Model -> Model
moveOneCritter point model =
    let
        lilypad =
            getLilypad point model.world

        ( move, newSeed ) =
            getCritterMove point model.world model.seed

        newModel =
            case move of
                Nothing ->
                    model

                Just ( _, newPoint ) ->
                    let
                        ( x, y ) =
                            newPoint

                        newWorld =
                            model.world
                                |> Dict.remove point
                                |> Dict.insert ( x, y ) (makePad x y lilypad.critter)
                    in
                        { model | world = newWorld, seed = newSeed }
    in
        newModel


moveCritters : Model -> Model
moveCritters model =
    List.foldl
        (\point acc ->
            moveOneCritter point acc
        )
        model
        (Dict.keys model.world)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- use current time for initial random seed
        OnTime t ->
            ( seedWorld (round t) model, Cmd.none )

        Reset ->
            ( seedWorld (round model.time) model, Cmd.none )

        Tick t ->
            let
                newModel =
                    moveCritters model
            in
                ( { newModel | time = t }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : model -> Sub Msg
subscriptions model =
    every second Tick
