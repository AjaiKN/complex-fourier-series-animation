module Draw exposing (Model, Msg(..), getFunction, init, update, view)

import Array exposing (Array)
import Browser
import Complex exposing (..)
import Helpers exposing (..)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (id)
import Json.Decode as Decode
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, viewBox, width, x, y)
import Svg.Events as Events
import Time



--MAIN


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
    { points : Array Complex
    }


init : Model
init =
    HaventStartedYet



-- UPDATE


type Msg
    = NewPoint Float Float
    | SwitchToNormalModel (Array Complex)
    | StartDrawing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case ( msg, model ) of
        ( NewPoint re im, Drawing mod ) ->
            Drawing
                { mod
                    | points =
                        Array.push
                            (complex
                                (coordTransformInverse 0 zoom re)
                                (coordTransformInverse 0 zoom im)
                            )
                            mod.points
                }

        ( StartDrawing, HaventStartedYet ) ->
            Drawing (DrawingModel Array.empty)

        --Handled in Main
        ( SwitchToNormalModel _, Drawing _ ) ->
            model

        _ ->
            model
    , Cmd.none
    )


getFunction : Array Complex -> Float -> Complex
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
        [ p []
            [ Html.h4
                [ Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "color" "red"
                ]
              <|
                List.singleton <|
                    text <|
                        case model of
                            HaventStartedYet ->
                                "Click once to start drawing. "

                            Drawing _ ->
                                "Click again to stop drawing. "
            , div [ Html.Attributes.style "text-align" "center" ]
                [ text "(This probably won't work on touch devices)" ]
            ]
        , svg
            [ viewBox "-10 16 120 200"
            , width "100%"
            , height "2000"
            , Events.on "svgmousemove" <|
                Decode.map2 NewPoint
                    (Decode.at [ "detail", "x" ] Decode.float)
                    (Decode.at [ "detail", "y" ] Decode.float)
            , id "drawable-svg"
            , Events.onClick <|
                case model of
                    HaventStartedYet ->
                        StartDrawing

                    Drawing mod ->
                        SwitchToNormalModel mod.points
            ]
            [ --rectangle to create black background
              rect [ fill "black", x "-100", y "-100", width "500", height "500" ] []
            , case model of
                Drawing mod ->
                    plotFunction "green" offset zoom (getFunction mod.points)

                HaventStartedYet ->
                    div [] []
            ]
        ]


offset =
    zero


zoom =
    2
