module Grid exposing (viewGrid)

import Color exposing (tileColor)
import Element exposing (Element, centerX, centerY, clip, el, fill, height, padding, px, rgb255, rgba255, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Tile exposing (Tile, baseTiles)
import Util


type alias Model =
    List Tile


viewGrid : Model -> Html msg
viewGrid model =
    let
        grid =
            Util.partitionEvery 4 <|
                List.map (\idx -> Util.find (\tile -> tile.position == idx) model) baseTiles
    in
    Element.layout [] <|
        el
            [ centerX
            , centerY
            , Background.color (rgb255 187 173 160)
            , padding 15
            , Border.rounded 6
            ]
        <|
            Element.column
                [ spacing 15 ]
            <|
                List.map gridRow grid


gridRow : List (Maybe Tile) -> Element msg
gridRow cells =
    Element.row [ spacing 15 ] <|
        List.map viewCell cells


viewCell : Maybe Tile -> Element msg
viewCell tile =
    el
        [ Border.rounded 3
        , Background.color (rgba255 240 227 213 0.35)
        , height (px 100)
        , width (px 100)
        , clip
        ]
    <|
        case tile of
            Nothing ->
                Element.none

            Just cell ->
                viewFilledCell cell.value


viewFilledCell : Int -> Element msg
viewFilledCell num =
    el
        [ width fill
        , height fill
        , Background.color (tileColor num)
        ]
    <|
        el
            [ centerY
            , width fill
            , Font.center
            , Font.size
                (if num >= 1000 then
                    35

                 else if num >= 100 then
                    45

                 else
                    55
                )
            , Font.color (rgb255 119 110 101)
            , Font.family
                [ Font.typeface "Helvetica Neue"
                , Font.typeface "Arial"
                , Font.sansSerif
                ]
            , Font.bold
            ]
            (text <| String.fromInt num)
