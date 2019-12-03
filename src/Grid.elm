module Grid exposing (viewGrid)

import Color exposing (tileColor)
import Html exposing (Attribute, Html, div, p, text)
import Html.Attributes exposing (..)
import Html.Keyed as Keyed
import Tile exposing (Tile, col, row)


type alias Model =
    List Tile


viewGrid : Model -> Html msg
viewGrid model =
    div [ style "display" "flex", style "justify-content" "center", style "align-items" "center", style "height" "100vh" ] <|
        [ div
            [ style "background-color" "rgb(187 173 160)"
            , style "width" "475px"
            , style "height" "475px"
            , style "border-radius" "6px"
            ]
          <|
            [ Keyed.ul
                [ style "width" "100%"
                , style "height" "100%"
                , style "position" "relative"
                , style "margin" "0"
                , style "padding" "0"
                ]
              <|
                gridBackground
                    :: List.map viewFilledCell model
            ]
        ]


gridBackground : ( String, Html msg )
gridBackground =
    ( "Background"
    , div
        [ style "display" "flex"
        , style "flex-direction" "column" --spacing 15
        , style "position" "absolute"
        , style "top" "0"
        , style "bottom" "0"
        , style "left" "0"
        , style "right" "0"
        ]
      <|
        List.repeat 4 <|
            div
                [ style "display" "flex" -- spacing 15
                , style "width" "100%"
                ]
            <|
                List.repeat
                    4
                    viewEmptyCell
    )


viewEmptyCell : Html msg
viewEmptyCell =
    div
        [ style "border-radius" "3px"
        , style "background-color" "rgba(240, 227, 213, 0.35)"
        , style "height" "100px"
        , style "width" "100px"
        , style "overflow" "none"
        , style "margin-left" "15px"
        , style "margin-top" "15px"
        ]
        []


viewFilledCell : Tile -> ( String, Html msg )
viewFilledCell tile =
    ( String.fromInt tile.slug
    , div
        [ style "border-radius" "3px"
        , style "background-color" (tileColor tile.value)
        , style "height" "100px"
        , style "width" "100px"
        , style "overflow" "none"
        , style "transition" "transform 100ms"
        , style "position" "absolute"
        , style "display" "flex"
        , style "align-items" "center"
        , adjustPosition tile
        ]
      <|
        [ div
            [ style "width" "100%"
            , style "text-align" "center"
            , style "font-size"
                (if tile.value >= 1000 then
                    "35px"

                 else if tile.value >= 100 then
                    "45px"

                 else
                    "55px"
                )
            , style "color" "rgb(119,110,101)"
            , style "font-family" "Helvetica Neue, Arial, sans-serif"
            , style "font-weight" "bold"
            ]
            [ text <| String.fromInt tile.value ]
        ]
    )


adjustPosition : Tile -> Attribute msg
adjustPosition tile =
    let
        x =
            String.fromInt ((col tile * 115) + 15) ++ "px"

        y =
            String.fromInt ((row tile * 115) + 15) ++ "px"
    in
    style "transform" ("translate3d(" ++ x ++ ", " ++ y ++ ", 0)")
