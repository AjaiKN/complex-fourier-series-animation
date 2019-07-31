module Normal exposing (FunctionName(..), Model, Msg(..), average, backAndForthTermNum, constantN, getFunction, init, main, makeCircle, makeLine, myRange, subscriptions, sumToTerm, term, update, view)

import Array exposing (Array)
import Browser
import Complex exposing (..)
import Dict exposing (Dict)
import Helpers exposing (..)
import Html exposing (Html, div, input, label, option, p, select, span, text)
import Html.Attributes exposing (checked, class, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy
import Svg exposing (Svg, circle, line, polygon, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import Task
import Time



--FOURIER SERIES CALCULATIONS


{-| All the possible functions in the dropdown.
-}
type FunctionName
    = SquareFunction
    | CosFunction
    | SinFunction2D
    | StepFunction
    | Line
    | Parabola
    | CustomFunction (Float -> Complex)


strToFunctionName : String -> FunctionName
strToFunctionName str =
    case str of
        "SquareFunction" ->
            SquareFunction

        "CosFunction" ->
            CosFunction

        "SinFunction2D" ->
            SinFunction2D

        "StepFunction" ->
            StepFunction

        "Line" ->
            Line

        "Parabola" ->
            Parabola

        _ ->
            StepFunction


{-| Apply one of the functions that has a FunctionName.
-}
getFunction : FunctionName -> Float -> Complex
getFunction funName t1 =
    let
        t =
            t1 - toFloat (floor t1)
    in
    case funName of
        SquareFunction ->
            complex
                (if t <= 0.25 then
                    1

                 else if t <= 0.5 then
                    3 - 8 * t

                 else if t <= 0.75 then
                    -1

                 else
                    -7 + 8 * t
                )
                (if t <= 0.25 then
                    -1 + 8 * t

                 else if t <= 0.5 then
                    1

                 else if t <= 0.75 then
                    5 - 8 * t

                 else
                    -1
                )

        CosFunction ->
            t |> turns |> cos |> real

        SinFunction2D ->
            complex
                (t * 4 - 2)
                (t * 3 |> turns |> sin)

        StepFunction ->
            if t < 0.5 then
                real 1

            else
                real -1

        Line ->
            complex (2 * t - 1) (2 * t - 1)

        Parabola ->
            complex (t * 2 - 1) (2 * (t * 2 - 1) ^ 2 - 1)

        CustomFunction fun ->
            fun t


{-| The range of values used as inputs for the averages calculated by constantN.
-}
myRange : Array Float
myRange =
    Array.fromList <| List.map (toFloat >> (*) 0.001) (List.range 0 999)


{-| Mean of an Array of Complex numbers (used by constantN).
-}
average : Array Complex -> Complex
average arr =
    divide (Array.foldl add zero arr) (Array.length arr |> toFloat |> real)


{-| Calculate the constants used for the terms of the Fourier series.
c\_n = integral from 0 to 1 of (exp(-2pi\_n\_t)\*f(t)) dt
To estimate this integral, we take the average of the expression as t ranges
from 0 to 1.
-}
constantN : (Float -> Complex) -> Int -> Complex
constantN f n =
    average <|
        Array.map
            (\t -> multiply (f t) (exp (imaginary (-2.0 * pi * toFloat n * t))))
            myRange


{-| Memoize all the values of constantN we're going to use and put them into
a Dict so that we don't have to calculate them multiple times.
-}
getDict : FunctionName -> Dict Int Complex
getDict funName =
    Dict.fromList <|
        List.map (\n -> ( n, constantN (getFunction funName) n )) (List.range -50 50)


{-| Calculate the nth term of the Fourier series. (n can be negative.)
nth term = c\_n \* exp(2\_pi\_)
-}
term : Dict Int Complex -> Int -> Float -> Complex
term dict n t =
    multiply (Dict.get n dict |> Maybe.withDefault zero) (exp (imaginary (2.0 * pi * toFloat n * t)))


{-| So that we include the negative terms too.
0, -1, 1, -2, 2, -3, 3, ...
-}
backAndForthTermNum : Int -> Int
backAndForthTermNum n =
    --0,1,-1,2,-2,3,-3,...
    if modBy 2 n == 0 then
        n // 2

    else
        -(n // 2 + 1)


{-| Find the Fourier series value to the nth term, except going back and forth
using backAndForthTermNum.
This is the position of the head of the nth vector.
-}
sumToTerm : Dict Int Complex -> Int -> Float -> Complex
sumToTerm dict n t =
    List.foldl
        (\newN prevSum ->
            add prevSum <|
                term dict (backAndForthTermNum newN) t
        )
        zero
        (List.range 0 n)



-- MAIN


main : Program () Model Msg
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
    , functionName : FunctionName
    , constantsDict : Dict Int Complex
    , showCircles : Bool
    , showIntendedShape : Bool
    , showTracedShape : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        defaultFunction =
            StepFunction
    in
    ( { originalTime = -100
      , sinceStart = 0
      , speed = "8"
      , numVectors = "40"
      , zoom = "2"
      , followFinalPoint = False
      , functionName = defaultFunction
      , constantsDict = getDict defaultFunction
      , showCircles = True
      , showIntendedShape = True
      , showTracedShape = True
      }
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
    | ChangeFunction FunctionName
    | ToggleShowCircles
    | ToggleShowIntendedShape
    | ToggleShowTracedShape
    | SwitchToDrawMode


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

        ToggleShowCircles ->
            ( { model | showCircles = not model.showCircles }
            , Cmd.none
            )

        ToggleShowIntendedShape ->
            ( { model | showIntendedShape = not model.showIntendedShape }
            , Cmd.none
            )

        ToggleShowTracedShape ->
            ( { model | showTracedShape = not model.showTracedShape }
            , Cmd.none
            )

        ChangeFunction functionName ->
            ( { model
                | sinceStart = 0
                , functionName = functionName
                , constantsDict = getDict functionName
              }
            , Task.perform InitialTime Time.now
            )

        -- This is handled in Main
        SwitchToDrawMode ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


{-| Send a Tick message every 16 milliseconds.
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 16 Tick



-- VIEW


view : Model -> Html Msg
view ({ speed, numVectors, zoom, followFinalPoint, showCircles, showIntendedShape, showTracedShape, functionName } as model) =
    let
        isCustomFunction =
            case functionName of
                CustomFunction _ ->
                    True

                _ ->
                    False
    in
    div []
        [ Html.Lazy.lazy8 viewInputs speed numVectors zoom followFinalPoint showCircles showIntendedShape showTracedShape isCustomFunction
        , viewAnimation model
        ]


viewInputs : String -> String -> String -> Bool -> Bool -> Bool -> Bool -> Bool -> Html Msg
viewInputs speed numVectors zoom followFinalPoint showCircles showIntendedShape showTracedShape isCustomFunction =
    divClass "row"
        [ divClass "col"
            [ functionDropdown isCustomFunction
            , Html.button [ onClick SwitchToDrawMode, class "btn btn-primary" ]
                [ text
                    (if isCustomFunction then
                        "Draw another function!"

                     else
                        "Draw your own function!"
                    )
                ]
            , numInput NumVectors numVectors "1" <| colorText "red" "Number of spinning vectors (max = 100)"
            , input
                [ type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "100"
                , value numVectors
                , onInput NumVectors
                ]
                []
            , p [] [ text "Try starting with one ", colorText "red" "vector ", text "and increasing one at a time." ]
            ]
        , divClass "col"
            [ numInput Speed speed "any" <| text "Speed (cycles per minute)"
            , numInput Zoom zoom "any" <| text "Zoom"
            , checkbox ToggleFollowFinalPoint followFinalPoint "green" "Follow green point (This might slow down some devices if you're showing intended or traced shapes.)"
            ]
        , divClass "col"
            [ checkbox ToggleShowCircles showCircles "orange" "Show orange circles"
            , checkbox ToggleShowIntendedShape showIntendedShape "green" "Show intended shape (green curve)"
            , checkbox ToggleShowTracedShape showTracedShape "blue" "Show traced shape (blue curve)"
            ]
        ]


checkbox : msg -> Bool -> String -> String -> Html msg
checkbox changer val color lab =
    divClass "form-check"
        [ input [ class "form-check-input", type_ "checkbox", value "", onClick changer, checked val ] []
        , label [ class "form-check-label" ] [ colorText color lab ]
        ]


colorText : String -> String -> Html msg
colorText color text_ =
    Html.node "font" [ Html.Attributes.attribute "color" color ] [ text text_ ]


functionDropdown : Bool -> Html Msg
functionDropdown isCustomFunction =
    div []
        [ label [] [ text "Function: " ]
        , select
            [ onInput functionNameStrToMsg ]
            [ option [ value "StepFunction" ] [ text "Step function" ]
            , option [ value "SquareFunction" ] [ text "Square function" ]
            , option [ value "CosFunction" ] [ text "1D Cosine" ]
            , option [ value "SinFunction2D" ] [ text "2D Sine" ]
            , option [ value "Line" ] [ text "Line" ]
            , option [ value "Parabola" ] [ text "Parabola" ]
            , option [ value "SwitchToDrawMode", Html.Attributes.selected isCustomFunction ] [ text "* Draw your own function! *" ]
            ]
        ]


functionNameStrToMsg str =
    case str of
        "SwitchToDrawMode" ->
            SwitchToDrawMode

        _ ->
            ChangeFunction (strToFunctionName str)


viewAnimation : Model -> Html msg
viewAnimation ({ sinceStart, followFinalPoint, functionName, constantsDict, showCircles, showIntendedShape, showTracedShape } as model) =
    let
        time =
            toFloat sinceStart / 1000 / 60 * speed

        { speed, numVectors, zoom } =
            getOptions model

        final =
            numVectors - 1

        finalPoint =
            sumToTerm constantsDict (final + 1) time

        offset =
            if followFinalPoint then
                finalPoint

            else
                zero
    in
    svg
        [ viewBox "-10 16 120 200"
        , width "100%"
        , height "2000"
        ]
    <|
        [ --rectangle to create black background
          rect [ fill "black", x "-100", y "-100", width "500", height "500" ] []

        --draw intended function (if box is checked)
        , if showIntendedShape then
            Html.Lazy.lazy3 plotIntendedFunction offset zoom functionName

          else
            div [] []

        --draw traced function (if box is checked)
        , if showTracedShape then
            Html.Lazy.lazy4 plotEstimatedFunction offset zoom constantsDict final

          else
            div [] []
        ]
            ++ List.concatMap
                (\n ->
                    let
                        current =
                            sumToTerm constantsDict n time

                        distanceToNext =
                            (Complex.toPolar (term constantsDict (backAndForthTermNum (n + 1)) time)).abs
                    in
                    --Don't bother drawing the vector if the magnitued is too small
                    if distanceToNext < 0.0001 then
                        []

                    else
                        [ --Draw vector
                          makeLine
                            offset
                            current
                            (sumToTerm constantsDict (n + 1) time)
                            zoom

                        --Draw circle
                        , if showCircles then
                            makeCircle offset current distanceToNext "orange" "none" zoom

                          else
                            div [] []

                        --Draw point (tiny circle)
                        , makeCircle offset current (0.015 / 2 * zoom) "none" "blue" zoom
                        ]
                )
                (List.range 0 final)
            ++ [ makeCircle offset finalPoint (0.03 / 2 * zoom) "none" "green" zoom
               ]


makeCircle : Complex -> Complex -> Float -> String -> String -> Float -> Svg msg
makeCircle offset a radius color fill_ zoom =
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
        , fill fill_
        , strokeWidth "0.06"
        ]
        []


makeLine : Complex -> Complex -> Complex -> Float -> Svg msg
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
        , strokeWidth "0.3"
        ]
        []


plotFunction : String -> Complex -> Float -> (Float -> Complex) -> Svg msg
plotFunction color offset zoom function =
    let
        offsetCartesian =
            toCartesian offset

        range =
            List.map (toFloat >> (*) (1 / 1000)) (List.range 0 1000)

        pts =
            List.map (function >> toCartesian) range

        pointsAsStrings =
            List.map
                (\{ re, im } ->
                    coordTransform offsetCartesian.re zoom re
                        ++ ","
                        ++ coordTransform offsetCartesian.im zoom im
                )
                pts

        pointsString =
            String.join " " pointsAsStrings
    in
    polygon [ points pointsString, strokeWidth "0.35", stroke color, fill "none" ] []



--The plotIntendedFunction and plotEstimatedFunction functions are so that
--we can use Html.Lazy so that we don't have to recalculate these polygons
--every frame.


plotIntendedFunction : Complex -> Float -> FunctionName -> Svg msg
plotIntendedFunction offset zoom functionName =
    plotFunction "green" offset zoom (getFunction functionName)


plotEstimatedFunction : Complex -> Float -> Dict Int Complex -> Int -> Svg msg
plotEstimatedFunction offset zoom constantsDict final =
    plotFunction "blue" offset zoom (sumToTerm constantsDict (final + 1))


numInput : (String -> Msg) -> String -> String -> Html Msg -> Html Msg
numInput changer val step_ lab =
    Html.span []
        [ label [] [ lab ]
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


divClass : String -> List (Html msg) -> Html msg
divClass c =
    div [ class c ]



--Float and String


strToFloat : String -> Float
strToFloat s =
    case String.toFloat s of
        Just f ->
            f

        Nothing ->
            0


strToInt : String -> Int
strToInt =
    Maybe.withDefault 0 << String.toInt



-- Get String options as Floats


type alias Options =
    { speed : Float
    , numVectors : Int
    , zoom : Float
    }


getOptions : Model -> Options
getOptions { speed, numVectors, zoom } =
    { speed = strToFloat speed
    , numVectors =
        if strToInt numVectors <= 100 && strToInt numVectors >= 0 then
            strToInt numVectors

        else if strToInt numVectors > 100 then
            100

        else
            0
    , zoom = strToFloat zoom
    }
