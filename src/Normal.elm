module Normal exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Array exposing (Array)
import Browser
import Browser.Events
import Complex exposing (..)
import Dict exposing (Dict)
import Fourier exposing (MemoizedConstants, getMemoizedConstantsDict, sumToTerm, term)
import FunctionName exposing (FunctionName)
import Helpers exposing (..)
import Html exposing (Html, div, input, label, option, p, select, span, text)
import Html.Attributes exposing (checked, class, id, step, type_, value)
import Html.Events as Events exposing (onClick, onInput)
import Html.Lazy
import Json.Decode as Decode
import Svg exposing (Svg, circle, line, polygon, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, strokeWidth, viewBox, width, x, x1, x2, y, y1, y2)
import Task
import Time



--FOURIER SERIES CALCULATIONS


getMemoizedEstimatedFunctionValuesList : MemoizedConstants -> Int -> List Complex
getMemoizedEstimatedFunctionValuesList memoizedConstants numVectors =
    List.map (sumToTerm memoizedConstants numVectors) Helpers.rangeForPlottingFunctions


getMemoizedIntendedFunctionValuesList : FunctionName -> List Complex
getMemoizedIntendedFunctionValuesList functionName =
    List.map (FunctionName.getFunction functionName) Helpers.rangeForPlottingFunctions



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
    { time : Float
    , speed : String
    , numVectors : String
    , zoom : String
    , followFinalPoint : Offsetting
    , functionName : FunctionName
    , showCircles : Bool
    , showIntendedShape : Bool
    , showTracedShape : Bool

    -- For performance only: so that we don't have to recompute stuff
    , memoizedConstantsDict : MemoizedConstants
    , memoizedEstimatedFunctionValuesList : List Complex
    , memoizedIntendedFunctionValuesList : List Complex
    }


type Offsetting
    = FollowFinalPoint
    | ConstantOffset Complex


init : () -> ( Model, Cmd Msg )
init _ =
    let
        defaultFunction =
            FunctionName.defaultFunction

        memoizedConstantsDict =
            getMemoizedConstantsDict defaultFunction

        numVectors =
            40
    in
    ( { time = 0
      , speed = "8"
      , numVectors = String.fromFloat numVectors
      , zoom = "2"
      , followFinalPoint = ConstantOffset zero
      , functionName = defaultFunction
      , showCircles = True
      , showIntendedShape = True
      , showTracedShape = True
      , memoizedConstantsDict = memoizedConstantsDict
      , memoizedEstimatedFunctionValuesList = getMemoizedEstimatedFunctionValuesList memoizedConstantsDict numVectors
      , memoizedIntendedFunctionValuesList = getMemoizedIntendedFunctionValuesList defaultFunction
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Tick Float
    | Speed String
    | NumVectors String
    | Zoom String
    | ToggleFollowFinalPoint
    | ChangeFunction FunctionName
    | ToggleShowCircles
    | ToggleShowIntendedShape
    | ToggleShowTracedShape
    | SwitchToDrawMode
    | ChangeOffsetBy Float Float
    | ZoomIn
    | ZoomOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        -- deltaTime is time in seconds since previous frame
        Tick deltaTime ->
            let
                { speed } =
                    getOptions model
            in
            { model
                | time = model.time + speed / 60 / 1000 * deltaTime
            }

        Speed s ->
            { model | speed = s }

        NumVectors s ->
            let
                newMod =
                    { model
                        | numVectors = s
                    }

                { numVectors } =
                    getOptions newMod
            in
            { newMod
                | memoizedEstimatedFunctionValuesList = getMemoizedEstimatedFunctionValuesList model.memoizedConstantsDict numVectors
            }

        Zoom s ->
            { model | zoom = s }

        ToggleFollowFinalPoint ->
            { model
                | followFinalPoint =
                    case model.followFinalPoint of
                        FollowFinalPoint ->
                            ConstantOffset zero

                        ConstantOffset _ ->
                            FollowFinalPoint
            }

        ToggleShowCircles ->
            { model | showCircles = not model.showCircles }

        ToggleShowIntendedShape ->
            { model | showIntendedShape = not model.showIntendedShape }

        ToggleShowTracedShape ->
            { model | showTracedShape = not model.showTracedShape }

        ChangeFunction functionName ->
            let
                { numVectors } =
                    getOptions model

                memoizedConstantsDict =
                    getMemoizedConstantsDict functionName
            in
            { model
                | time = 0
                , functionName = functionName
                , memoizedConstantsDict = memoizedConstantsDict
                , memoizedEstimatedFunctionValuesList = getMemoizedEstimatedFunctionValuesList memoizedConstantsDict numVectors
                , memoizedIntendedFunctionValuesList = getMemoizedIntendedFunctionValuesList functionName
            }

        ChangeOffsetBy re im ->
            case model.followFinalPoint of
                FollowFinalPoint ->
                    model

                ConstantOffset offset ->
                    let
                        { zoom } =
                            getOptions model
                    in
                    { model
                        | followFinalPoint =
                            ConstantOffset <|
                                add offset <|
                                    complex
                                        (distTransformInverse zoom re)
                                        (distTransformInverse zoom im)
                    }

        ZoomIn ->
            let
                { zoom } =
                    getOptions model
            in
            { model | zoom = String.fromFloat <| zoom * 0.75 }

        ZoomOut ->
            let
                { zoom } =
                    getOptions model
            in
            { model | zoom = String.fromFloat <| zoom / 0.75 }

        -- This is handled in Main
        SwitchToDrawMode ->
            model

        NoOp ->
            model
    , Cmd.none
    )



-- SUBSCRIPTIONS


{-| Send a Tick message every millisecondsPerFrame milliseconds.
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown <|
            Decode.map
                (\s ->
                    case s of
                        "=" ->
                            ZoomIn

                        "+" ->
                            ZoomIn

                        "-" ->
                            ZoomOut

                        "_" ->
                            ZoomOut

                        _ ->
                            NoOp
                )
                (Decode.field "key" Decode.string)
        ]



-- VIEW


view : Model -> Html Msg
view ({ speed, numVectors, zoom, followFinalPoint, showCircles, showIntendedShape, showTracedShape, functionName } as model) =
    let
        isCustomFunction =
            FunctionName.isCustomFunction functionName
    in
    div []
        [ Html.Lazy.lazy8 viewInputs speed numVectors zoom followFinalPoint showCircles showIntendedShape showTracedShape isCustomFunction
        , viewAnimation model
        ]


viewInputs : String -> String -> String -> Offsetting -> Bool -> Bool -> Bool -> Bool -> Html Msg
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
            , numInputWithSlider NumVectors numVectors "0" "100" "1" <| colorText "red" "Number of spinning vectors (max = 100)"
            , p [] [ text "Try starting with one ", colorText "red" "vector ", text "and increasing one at a time." ]
            ]
        , divClass "col"
            [ numInputWithSlider Speed speed "0" "20" "any" <| text "Speed (cycles per minute)"
            , numInputWithSlider Zoom zoom "0.001" "3" "any" <|
                div []
                    [ text "Zoom"
                    , Html.button [ class "mx-1", onClick ZoomIn ] [ text "+" ]
                    , Html.button [ class "mx-1", onClick ZoomOut ] [ text "-" ]
                    ]
            , checkbox ToggleFollowFinalPoint (followFinalPoint == FollowFinalPoint) "green" "Follow green point (This might slow down some devices if you're showing intended or traced shapes.)"
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
            , option [ value "LemniscateOfBernoulli" ] [ text "Lemniscate of Bernoulli (infinity symbol)" ]
            , option [ value "SwitchToDrawMode", Html.Attributes.selected isCustomFunction ] [ text "* Draw your own function! *" ]
            ]
        ]


functionNameStrToMsg str =
    case str of
        "SwitchToDrawMode" ->
            SwitchToDrawMode

        _ ->
            ChangeFunction (FunctionName.fromString str)


viewAnimation : Model -> Html Msg
viewAnimation ({ time, followFinalPoint, showCircles, showIntendedShape, showTracedShape, memoizedConstantsDict, memoizedEstimatedFunctionValuesList, memoizedIntendedFunctionValuesList } as model) =
    let
        { speed, numVectors, zoom } =
            getOptions model

        finalPoint =
            sumToTerm memoizedConstantsDict numVectors time

        offset =
            case followFinalPoint of
                FollowFinalPoint ->
                    finalPoint

                ConstantOffset offset_ ->
                    offset_
    in
    svg
        [ viewBox "-10 16 120 200"
        , width "100%"
        , height "2000"
        , id
            (if followFinalPoint == FollowFinalPoint then
                "svg"

             else
                "draggable-svg"
            )
        , Events.on "svgdrag" <|
            Decode.map2 ChangeOffsetBy
                (Decode.at [ "detail", "x" ] Decode.float)
                (Decode.at [ "detail", "y" ] Decode.float)
        ]
    <|
        [ --rectangle to create black background
          rect [ fill "black", x "-100", y "-100", width "500", height "500" ] []

        --draw intended function (if box is checked)
        , if showIntendedShape then
            plotPoints "green" offset zoom memoizedIntendedFunctionValuesList

          else
            div [] []

        --draw traced function (if box is checked)
        , if showTracedShape then
            plotPoints "blue" offset zoom memoizedEstimatedFunctionValuesList

          else
            div [] []
        ]
            ++ List.concatMap
                (\n ->
                    let
                        current =
                            sumToTerm memoizedConstantsDict n time

                        distanceToNext =
                            (Complex.toPolar (term memoizedConstantsDict (n + 1) time)).abs
                    in
                    --Don't bother drawing the vector if the magnitued is too small
                    if distanceToNext < 0.0001 then
                        []

                    else
                        [ --Draw vector
                          makeLine
                            offset
                            current
                            (sumToTerm memoizedConstantsDict (n + 1) time)
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
                (List.range 0 (numVectors - 1))
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


numInput : (String -> Msg) -> String -> String -> Html Msg -> Html Msg
numInput changer val step_ lab =
    Html.span []
        [ label [] [ lab ]
        , divClass1 "input-group" <|
            input
                [ class "numberInput form-control"
                , onInput changer
                , type_ "number"
                , value val
                , step step_
                ]
                []
        ]


numInputWithSlider : (String -> Msg) -> String -> String -> String -> String -> Html Msg -> Html Msg
numInputWithSlider changer val min max step_ lab =
    div []
        [ numInput changer val step_ lab
        , divClass1 "input-group" <|
            input
                [ type_ "range"
                , class "form-control-range"
                , Html.Attributes.min min
                , Html.Attributes.max max
                , step step_
                , value val
                , onInput changer
                ]
                []
        ]


divClass : String -> List (Html msg) -> Html msg
divClass c =
    div [ class c ]


divClass1 : String -> Html msg -> Html msg
divClass1 c h =
    divClass c [ h ]



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
