module Helpers exposing (coordTransform, coordTransformInverse, distTransform, distTransformInverse, plotFunction)

import Complex exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


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


coordTransform : Float -> Float -> Float -> String
coordTransform offset zoom float =
    String.fromFloat ((float + zoom - offset) * 100 / zoom / 2)


coordTransformInverse : Float -> Float -> Float -> Float
coordTransformInverse offset zoom output =
    (output * 2 * zoom / 100) + offset - zoom


distTransform : Float -> Float -> String
distTransform zoom float =
    String.fromFloat (abs (float * 100 / zoom / 2))


distTransformInverse : Float -> Float -> Float
distTransformInverse zoom output =
    output * 2 * zoom / 100
