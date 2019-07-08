module Main exposing (Model, Msg(..), average, backAndForthTermNum, constantN, coordTransform, distTransform, init, main, makeCircle, makeLine, myRange, squareIm, squareRe, subscriptions, sumToTerm, term, theFunction, update, view)

import Array exposing (Array)
import Browser
import Complex exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, div, input, label, span, text)
import Html.Attributes exposing (class, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy)
import Svg exposing (Svg, circle, line, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, width, x, x1, x2, y, y1, y2)
import Task
import Time


squareRe : Float -> Float
squareRe t =
    if t <= 0.25 then
        1

    else if t <= 0.5 then
        3 - 8 * t

    else if t <= 0.75 then
        -1

    else
        -7 + 8 * t


squareIm : Float -> Float
squareIm t =
    if t <= 0.25 then
        -1 + 8 * t

    else if t <= 0.5 then
        1

    else if t <= 0.75 then
        5 - 8 * t

    else
        -1


squareFunction : Float -> Complex
squareFunction t =
    complex (squareRe t) (squareIm t)


cosFunction : Float -> Complex
cosFunction =
    turns >> cos >> real


stepFunction : Float -> Complex
stepFunction t =
    if t < 0.5 then
        real 1

    else
        real -1


sinFunction2D : Float -> Complex
sinFunction2D t =
    complex
        (t * 4 - 2)
        (t * 3 |> turns |> sin)


theFunction : Float -> Complex
theFunction t1 =
    let
        t =
            t1 - toFloat (floor t1)
    in
    stepFunction t


myRange : Array Float
myRange =
    Array.fromList <| List.map (toFloat >> (*) 0.001) (List.range 0 999)


average : Array Complex -> Complex
average arr =
    divide (Array.foldl add zero arr) (Array.length arr |> toFloat |> real)


constantN : (Float -> Complex) -> Int -> Complex
constantN f n =
    average <|
        Array.map
            (\t -> multiply (f t) (exp (imaginary (-2.0 * pi * toFloat n * t))))
            myRange


memoize : (comparable -> b) -> List comparable -> comparable -> b
memoize f vals =
    let
        dict : Dict comparable b
        dict =
            Dict.fromList <|
                List.map (\n -> ( n, f n )) vals
    in
    \inp ->
        case Dict.get inp dict of
            Just n ->
                n

            Nothing ->
                f inp


constantNMemoized : Int -> Complex
constantNMemoized =
    memoize (constantN theFunction) (List.range -200 200)


term : (Float -> Complex) -> Int -> Float -> Complex
term f n t =
    multiply (constantNMemoized n) (exp (imaginary (2.0 * pi * toFloat n * t)))


backAndForthTermNum : Int -> Int
backAndForthTermNum n =
    --0,1,-1,2,-2,3,-3,...
    if modBy 2 n == 0 then
        n // 2

    else
        -(n // 2 + 1)


sumToTerm : (Float -> Complex) -> Int -> Float -> Complex
sumToTerm f n t =
    List.foldl
        (\newN prevSum ->
            add prevSum <|
                term f (backAndForthTermNum newN) t
        )
        zero
        (List.range 0 n)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { originalTime : Int
    , sinceStart : Int
    , speed : String
    , numVectors : String
    , zoom : String
    , followFinalPoint : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model -100 0 (floatToStr 8) "40" "2" False
    , Task.perform InitialTime Time.now
    )



-- UPDATE


type Msg
    = InitialTime Time.Posix
    | Tick Time.Posix
    | Speed String
    | NumVectors String
    | Zoom String
    | ToggleFollowFinalPoint


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialTime t ->
            ( { model | originalTime = Time.posixToMillis t, sinceStart = 0 }
            , Cmd.none
            )

        Tick t ->
            ( { model | sinceStart = Time.posixToMillis t - model.originalTime }
            , Cmd.none
            )

        Speed s ->
            ( { model | speed = s }
            , Cmd.none
            )

        NumVectors s ->
            ( { model | numVectors = s }
            , Cmd.none
            )

        Zoom s ->
            ( { model | zoom = s }
            , Cmd.none
            )

        ToggleFollowFinalPoint ->
            ( { model | followFinalPoint = not model.followFinalPoint }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 10 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ lazy viewInputs model, viewAnimation model ]


viewInputs : Model -> Html Msg
viewInputs model =
    div []
        [ numInput Speed model.speed "Speed (cycles per minute)" "any"
        , numInput NumVectors model.numVectors "Number of spinning vectors" "1"
        , numInput Zoom model.zoom "Zoom" "any"
        , checkbox ToggleFollowFinalPoint "Follow final point"
        ]


checkbox : msg -> String -> Html msg
checkbox changer lab =
    divClass "form-check"
        [ input [ class "form-check-input", type_ "checkbox", value "", onClick changer ] []
        , label [ class "form-check-label" ] [ text lab ]
        ]


viewAnimation : Model -> Html Msg
viewAnimation ({ sinceStart, followFinalPoint } as model) =
    let
        time =
            toFloat sinceStart / 1000 / 60 * speed

        { speed, numVectors, zoom } =
            getOptions model

        final =
            numVectors - 1

        finalPoint =
            sumToTerm theFunction (final + 1) time

        offset =
            if followFinalPoint then
                finalPoint

            else
                zero

        offsetCartesian =
            toCartesian offset
    in
    svg [ width "900", height "900" ] <|
        rect
            [ x (coordTransform offsetCartesian.re zoom -1)
            , y (coordTransform offsetCartesian.im zoom -1)
            , width (distTransform zoom 2)
            , height (distTransform zoom 2)
            , stroke "green"
            , fill "none"
            ]
            []
            :: List.concatMap
                (\n ->
                    let
                        current =
                            sumToTerm theFunction n time

                        distanceToNext =
                            (Complex.toPolar (term theFunction (backAndForthTermNum (n + 1)) time)).abs
                    in
                    if distanceToNext < 0.0001 then
                        []

                    else
                        [ makeLine
                            offset
                            current
                            (sumToTerm theFunction (n + 1) time)
                            zoom
                        , makeCircle offset current distanceToNext "red" "none" zoom
                        , makeCircle offset current (0.015 / 1.5 * zoom) "none" "blue" zoom
                        ]
                )
                (List.range 0 final)
            ++ [ makeCircle offset finalPoint (0.03 / 1.5 * zoom) "none" "green" zoom ]


makeCircle : Complex -> Complex -> Float -> String -> String -> Float -> Svg Msg
makeCircle offset a radius color f zoom =
    let
        c =
            toCartesian a

        offsetCartesian =
            toCartesian offset
    in
    circle
        [ cx (coordTransform offsetCartesian.re zoom c.re)
        , cy (coordTransform offsetCartesian.im zoom c.im)
        , r (distTransform zoom radius)
        , stroke color
        , fill f
        ]
        []


makeLine : Complex -> Complex -> Complex -> Float -> Svg Msg
makeLine offset a1 a2 zoom =
    let
        c1 =
            toCartesian a1

        c2 =
            toCartesian a2

        offsetCartesian =
            toCartesian offset
    in
    line
        [ x1 (coordTransform offsetCartesian.re zoom c1.re)
        , y1 (coordTransform offsetCartesian.im zoom c1.im)
        , x2 (coordTransform offsetCartesian.re zoom c2.re)
        , y2 (coordTransform offsetCartesian.im zoom c2.im)
        , stroke "red"
        ]
        []


coordTransform : Float -> Float -> Float -> String
coordTransform offset zoom f =
    String.fromFloat ((f + zoom - offset) * 100 / zoom / 2) ++ "%"


distTransform : Float -> Float -> String
distTransform zoom f =
    String.fromFloat (f * 100 / zoom / 2) ++ "%"


numInput : (String -> Msg) -> String -> String -> String -> Html Msg
numInput changer val lab step_ =
    Html.span []
        [ label [] [ text lab ]
        , divClass "input-group"
            [ input
                [ class "numberInput form-control"
                , onInput changer
                , type_ "number"
                , value val
                , step step_
                ]
                []
            ]
        ]


divClass c =
    div [ class c ]


divClass1 c thing =
    div [ class c ] [ thing ]



--Float and String


strToFloat : String -> Float
strToFloat s =
    case String.toFloat s of
        Just f ->
            f

        Nothing ->
            0


strToInt : String -> Int
strToInt s =
    case String.toInt s of
        Just f ->
            f

        Nothing ->
            0


floatToStr : Float -> String
floatToStr =
    String.fromFloat


type alias Options =
    { speed : Float
    , numVectors : Int
    , zoom : Float
    }


getOptions : Model -> Options
getOptions { speed, numVectors, zoom } =
    { speed = strToFloat speed
    , numVectors =
        if strToInt numVectors < 400 && strToInt numVectors >= 0 then
            strToInt numVectors

        else
            0
    , zoom = strToFloat zoom
    }
