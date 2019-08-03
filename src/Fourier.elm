module Fourier exposing (MemoizedConstants, getMemoizedConstants, sumToTerm, term)

import Array exposing (Array)
import Complex exposing (..)
import FunctionName exposing (FunctionName, getFunction)


type MemoizedConstants
    = MemoizedConstants (Array Complex)


{-| Memoize all the values of constantN we're going to use and put them into
an Array so that we don't have to calculate them multiple times.
-}
getMemoizedConstants : FunctionName -> MemoizedConstants
getMemoizedConstants funName =
    List.range 0 100
        |> List.map (\n -> constantN (FunctionName.getFunction funName) (backAndForthTermNum n))
        |> Array.fromList
        |> MemoizedConstants


{-| So that we include the negative terms too.
0, -1, 1, -2, 2, -3, 3, ...
-}
backAndForthTermNum : Int -> Int
backAndForthTermNum n =
    if modBy 2 n == 0 then
        n // 2

    else
        -(n // 2 + 1)


get : Int -> MemoizedConstants -> Complex
get n (MemoizedConstants arr) =
    Array.get n arr
        |> Maybe.withDefault zero


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


{-| Calculate the nth term of the Fourier series, except
using backAndForthTermNum.
nth term = c\_n \* exp(2pi \* i \* nt)
-}
term : MemoizedConstants -> Int -> Float -> Complex
term memoizedConstants n t =
    multiply (get n memoizedConstants) (exp (imaginary (2.0 * pi * toFloat (backAndForthTermNum n) * t)))


{-| Find the Fourier series value to the nth term, except going back and forth
using backAndForthTermNum.
This is the position of the head of the nth vector.
-}
sumToTerm : MemoizedConstants -> Int -> Float -> Complex
sumToTerm memoizedConstants n t =
    List.foldl
        (\newN prevSum ->
            add prevSum <|
                term memoizedConstants newN t
        )
        zero
        (List.range 0 n)
