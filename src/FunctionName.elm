module FunctionName exposing (FunctionName, defaultFunction, fromString, getFunction, isCustomFunction, makeCustomFunction)

{-| Represents all the possible functions in the "Function: " dropdown.
-}

import Complex exposing (..)


type FunctionName
    = SquareFunction
    | CosFunction
    | SinFunction2D
    | StepFunction
    | Line
    | Parabola
    | LemniscateOfBernoulli
    | CustomFunction (Float -> Complex)


makeCustomFunction : (Float -> Complex) -> FunctionName
makeCustomFunction =
    CustomFunction


defaultFunction : FunctionName
defaultFunction =
    StepFunction


isCustomFunction : FunctionName -> Bool
isCustomFunction funName =
    case funName of
        CustomFunction _ ->
            True

        _ ->
            False


fromString : String -> FunctionName
fromString str =
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

        "LemniscateOfBernoulli" ->
            LemniscateOfBernoulli

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

        LemniscateOfBernoulli ->
            let
                t2 =
                    t * 2 * pi

                x =
                    2 / 1.2 * sqrt 2 * cos t2 / ((sin t2 * sin t2) + 1)
            in
            complex x (x * sin t2)

        CustomFunction fun ->
            fun t
