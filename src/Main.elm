module Main exposing (main)

import Browser
import Draw
import FunctionName
import Html exposing (Html)
import Normal



--MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = NormalModel Normal.Model
    | DrawModel Draw.Model


init : () -> ( Model, Cmd Msg )
init () =
    Normal.init ()
        |> Tuple.mapBoth NormalModel (Cmd.map NormalMsg)



-- UPDATE


type Msg
    = NormalMsg Normal.Msg
    | DrawMsg Draw.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( NormalMsg Normal.SwitchToDrawMode, NormalModel mod ) ->
            ( DrawModel Draw.init, Cmd.none )

        ( DrawMsg (Draw.SwitchToNormalModel points), DrawModel mod ) ->
            let
                updateIt : Normal.Model -> Normal.Model
                updateIt =
                    Normal.update
                        (Normal.ChangeFunction <| FunctionName.makeCustomFunction <| Draw.getFunction points)
                        >> Tuple.first
            in
            Normal.init () |> Tuple.mapBoth (updateIt >> NormalModel) (Cmd.map NormalMsg)

        ( NormalMsg ms, NormalModel mod ) ->
            Normal.update ms mod
                |> Tuple.mapBoth NormalModel (Cmd.map NormalMsg)

        ( DrawMsg ms, DrawModel mod ) ->
            Draw.update ms mod
                |> Tuple.mapBoth DrawModel (Cmd.map DrawMsg)

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        NormalModel mod ->
            Normal.subscriptions mod
                |> Sub.map NormalMsg

        DrawModel mod ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        NormalModel mod ->
            Normal.view mod
                |> Html.map NormalMsg

        DrawModel mod ->
            Draw.view mod
                |> Html.map DrawMsg
