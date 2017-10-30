module Main exposing (main)

import Types exposing (..)
import Html exposing (Html, div, p, text, h1, img, button, input)
import Html.Attributes exposing (..)
import Random
import Random.Pcg
import Dict exposing (Dict)
import Time exposing (every, second, millisecond, inMilliseconds, inSeconds)
import Task
import Random.List
import Date exposing (fromTime)
import Html.Events exposing (onClick, onInput)


--import Set
--import Random.Set
--import Tuple
{-
   World is 64 x 128 or 8192 cells, contains 3000 frogs and 3000 turtles
-}


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    let
        emptyWorld =
            { width = 64
            , height = 128
            , world = Dict.empty
            , time = 0
            , randomPcgSeed = Random.Pcg.initialSeed 0
            , randomSeed = Random.initialSeed 0
            , images = False
            , critters = 6000
            }
    in
        ( emptyWorld, getTime )


makePad : Int -> Int -> Int -> Int -> Critter -> { critter : Critter, location : Point, neighboors : List Point }
makePad x y width height critter =
    let
        neighboors =
            case critter of
                Empty ->
                    []

                _ ->
                    makeNeighboors x y width height
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


makeNeighboors : Int -> Int -> Int -> Int -> List Point
makeNeighboors x y width height =
    let
        isOnMap x y =
            x >= 0 && x < width && y >= 0 && y < height
    in
        grid (x - 1) (x + 1) (y - 1) (y + 1)
            |> List.filter (\( x1, y1 ) -> ( x1, y1 ) /= ( x, y ) && isOnMap x1 y1)


getTime : Cmd Msg
getTime =
    Task.perform OnTime Time.now


seedWorld : Model -> Model
seedWorld model =
    if Dict.size model.world >= model.critters then
        model
    else
        let
            numPointsWanted =
                1000

            ( randomPoints, newSeed ) =
                (randomListOfPoints model.randomPcgSeed model.width model.height numPointsWanted)

            newWorld =
                enlargeWorld model.world randomPoints model.critters model.width model.height

            newModel =
                { model | world = newWorld, randomPcgSeed = newSeed }
        in
            if Dict.size newModel.world >= model.critters then
                newModel
            else
                seedWorld newModel


enlargeWorld : World -> List Point -> Int -> Int -> Int -> World
enlargeWorld world points critters width height =
    List.foldl
        (\( x, y ) acc ->
            let
                critter =
                    if Dict.size acc < critters // 2 then
                        Frog
                    else
                        Turtle
            in
                if Dict.member ( x, y ) acc == False && Dict.size acc < critters then
                    acc |> Dict.insert ( x, y ) (makePad x y width height critter)
                else
                    acc
        )
        world
        points


randomListOfPoints : Random.Pcg.Seed -> Int -> Int -> Int -> ( List Point, Random.Pcg.Seed )
randomListOfPoints seed width height len =
    let
        generatorPair =
            Random.Pcg.pair (Random.Pcg.int 0 (width - 1)) (Random.Pcg.int 0 (height - 1))

        generatorListOfPoints =
            Random.Pcg.list len generatorPair
    in
        Random.Pcg.step generatorListOfPoints seed



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

        determineImgSrc pad =
            case pad.critter of
                Frog ->
                    [ img [ src "images/icons8-Frog-48.png", height 25, width 25 ] [] ]

                Turtle ->
                    [ img [ src "images/icons8-Turtle-48.png", height 25, width 25 ] [] ]

                _ ->
                    [ img [ src "images/icons8-blank-48.png", height 25, width 25 ] [] ]

        color =
            Dict.get ( col, row ) model.world
                |> Maybe.map determineColor
                |> Maybe.withDefault "black"

        imgsrc =
            Dict.get ( col, row ) model.world
                |> Maybe.map determineImgSrc
                |> Maybe.withDefault [ text "" ]
    in
        if model.images then
            div
                [ style
                    [ ( "width", "25px" )
                    , ( "height", "25px" )
                    , ( "background-color", "black" )
                    , ( "display", "inline-block" )
                    , ( "border", "none" )
                    , ( "vertical-align", "top" )
                    ]
                ]
                imgsrc
        else
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
    let
        myStyle =
            if model.images then
                [ ( "height", "25px" ) ]
            else
                [ ( "height", "2px" ) ]
    in
        div
            [ style
                myStyle
            ]
            (List.map (viewCell model row) <| List.range 0 model.width)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Pond" ]
        , p []
            [ text <| toString <| fromTime <| inMilliseconds model.time
            ]
        , div
            []
            --[ style
            --    [ ( "padding", "8px" ), ( "background-image", "url(\"images/pond17.jpg\")" ) ]
            --]
            (List.map (viewRow model) <| List.range 0 model.height)
        , p []
            [ button [ onClick Reset ] [ text "Reset" ]
            ]
        , p []
            [ button [ onClick Dots ] [ text "Dots" ]
            ]
        , p []
            [ button [ onClick Images ] [ text "Images" ]
            ]
        , div []
            [ input
                [ type_ "range"
                , Html.Attributes.min "1000"
                , Html.Attributes.max "20000"
                , value <| toString model.critters
                , onInput UpdateCritters
                ]
                []
            , text <| toString model.critters
            , text " critters"
            ]
        , div []
            [ input
                [ type_ "range"
                , Html.Attributes.min "32"
                , Html.Attributes.max "256"
                , value <| toString model.width
                , onInput UpdateWidth
                ]
                []
            , text <| toString model.width
            , text " width"
            ]
        , div []
            [ input
                [ type_ "range"
                , Html.Attributes.min "32"
                , Html.Attributes.max "256"
                , value <| toString model.height
                , onInput UpdateHeight
                ]
                []
            , text <| toString model.height
            , text " height"
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
getCritterMove point world oldRandomSeed =
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
                    Random.step (Random.List.shuffle (getEmptyNeighboors point)) oldRandomSeed

                newPoint =
                    List.head result
            in
                ( newPoint, newSeed )
    in
        if (toFloat neighboorsSameKind / toFloat neighboorsTotal) < 0.3 then
            getRandomEmptyNeighboor point
        else
            ( Nothing, oldRandomSeed )


moveOneCritter : Point -> Model -> Model
moveOneCritter point model =
    let
        lilypad =
            getLilypad point model.world

        ( move, newRandomSeed ) =
            getCritterMove point model.world model.randomSeed

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
                                |> Dict.insert ( x, y ) (makePad x y model.width model.height lilypad.critter)
                    in
                        { model | world = newWorld, randomSeed = newRandomSeed }
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
            ( seedWorld { model | time = t, randomSeed = Random.initialSeed (round t), randomPcgSeed = Random.Pcg.initialSeed (round t) }, Cmd.none )

        Reset ->
            ( seedWorld { model | world = Dict.empty, randomSeed = Random.initialSeed (round model.time), randomPcgSeed = Random.Pcg.initialSeed (round model.time) }, Cmd.none )

        Tick t ->
            let
                newModel =
                    moveCritters model
            in
                ( { newModel | time = t }, Cmd.none )

        Dots ->
            ( { model | images = False }, Cmd.none )

        Images ->
            ( { model | images = True }, Cmd.none )

        UpdateCritters v ->
            let
                critters =
                    String.toInt v |> Result.withDefault 0

                newCritters =
                    Basics.min critters (round (0.9 * toFloat (model.width * model.height)))
            in
                ( { model | critters = newCritters }, Cmd.none )

        UpdateWidth v ->
            let
                width =
                    String.toInt v |> Result.withDefault 0

                newWidth =
                    Basics.max width (round (1.1 * (toFloat model.critters / toFloat model.height)))
            in
                ( { model | width = newWidth }, Cmd.none )

        UpdateHeight v ->
            let
                height =
                    String.toInt v |> Result.withDefault 0

                newHeight =
                    Basics.max height (round (1.1 * (toFloat model.critters / toFloat model.width)))
            in
                ( { model | height = height }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : model -> Sub Msg
subscriptions model =
    every second Tick
