module Draw exposing (Model, Msg(..), getFunction, init, main, update, view)

{-| This module controls the page when you're drawing the custom function.
-}

import Array exposing (Array)
import Browser
import Complex exposing (..)
import Helpers exposing (..)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (id)
import Html.Lazy
import Json.Decode as Decode
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, viewBox, width, x, y)
import Svg.Events as Events



--MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( init, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL


type Model
    = HaventStartedYet
    | Drawing DrawingModel


type alias DrawingModel =
    -- Array of points drawn
    Array Complex


init : Model
init =
    HaventStartedYet



-- UPDATE


type Msg
    = UserClickedOnceToStartDrawing
    | MouseMovedToDrawPoint Float Float
    | UserClickedSecondTimeToSwitchToNormalMode DrawingModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case ( msg, model ) of
        --Add point to points array
        ( MouseMovedToDrawPoint re im, Drawing points ) ->
            Drawing <|
                Array.push
                    (complex
                        (coordTransformInverse 0 zoom re)
                        (coordTransformInverse 0 zoom im)
                    )
                    points

        --Start drawing
        ( UserClickedOnceToStartDrawing, HaventStartedYet ) ->
            Drawing Array.empty

        --Handled in Main
        ( UserClickedSecondTimeToSwitchToNormalMode _, Drawing _ ) ->
            model

        _ ->
            model
    , Cmd.none
    )


{-| Converts a DrawingModel to a function that takes a Float from
0 to 1.
-}
getFunction : DrawingModel -> Float -> Complex
getFunction array =
    let
        len =
            Array.length array
    in
    if len == 0 then
        always zero

    else
        \t ->
            let
                ind =
                    modBy len <| floor <| t * toFloat len
            in
            Maybe.withDefault Complex.zero <| Array.get ind array



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.Lazy.lazy viewText (model /= HaventStartedYet)
        , svg
            [ viewBox "-10 16 120 200"
            , width "100%"
            , height "2000"
            , Events.on "svgmousemove" <|
                Decode.map2 MouseMovedToDrawPoint
                    (Decode.at [ "detail", "x" ] Decode.float)
                    (Decode.at [ "detail", "y" ] Decode.float)
            , id "drawable-svg"
            , Events.onClick <|
                case model of
                    HaventStartedYet ->
                        UserClickedOnceToStartDrawing

                    Drawing points ->
                        UserClickedSecondTimeToSwitchToNormalMode points
            ]
            [ --rectangle to create black background
              rect [ fill "black", x "-100", y "-100", width "500", height "500" ] []
            , case model of
                Drawing points ->
                    plotPoints "green" offset zoom (Array.toList points)

                HaventStartedYet ->
                    div [] []
            ]
        ]


viewText : Bool -> Html msg
viewText haveStartedDrawing =
    div []
        [ Html.h4
            [ Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "color" "red"
            ]
            [ text <|
                if haveStartedDrawing then
                    "Click again to stop drawing. "

                else
                    "Click once to start drawing. "
            ]
        , p [ Html.Attributes.style "text-align" "center" ]
            [ text "(This probably won't work on touch devices)" ]
        ]



-- CONSTANTS


offset : Complex
offset =
    zero


zoom : Float
zoom =
    2
