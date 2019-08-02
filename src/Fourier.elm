module Fourier exposing (backAndForthTermNum, getMemoizedConstantsDict, sumToTerm, term)

import Array exposing (Array)
import Complex exposing (..)
import Dict exposing (Dict)
import FunctionName exposing (FunctionName, getFunction)


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
getMemoizedConstantsDict : FunctionName -> Dict Int Complex
getMemoizedConstantsDict funName =
    Dict.fromList <|
        List.map (\n -> ( n, constantN (FunctionName.getFunction funName) n )) (List.range -50 50)


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
