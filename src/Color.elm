module Color exposing (tileColor)

import Element exposing (Color, fromRgb, rgb255, toRgb)


tileColor : Int -> Element.Color
tileColor num =
    let
        weight =
            logBase 2 (toFloat num) / logBase 2 2048

        minColor =
            rgb255 238 228 218

        maxColor =
            rgb255 237 194 46
    in
    mixColors maxColor minColor weight


mixColors : Color -> Color -> Float -> Color
mixColors color1 color2 weight =
    let
        ( rgb1, rgb2 ) =
            ( toRgb color1, toRgb color2 )

        weightedRgb =
            { red = (rgb1.red * weight) + (rgb2.red * 1 - weight)
            , green = (rgb1.green * weight) + (rgb2.green * 1 - weight)
            , blue = (rgb1.blue * weight) + (rgb2.blue * 1 - weight)
            , alpha = (rgb1.alpha * weight) + (rgb2.alpha * 1 - weight)
            }
    in
    fromRgb weightedRgb
