module Color exposing (tileColor)


tileColor : Int -> String
tileColor num =
    let
        weight =
            logBase 2 (toFloat num) / logBase 2 2048

        minColor =
            RGB 238 228 218

        maxColor =
            RGB 237 194 46

        mixedColor =
            mixColors maxColor minColor weight
    in
    "rgb(" ++ String.fromFloat mixedColor.red ++ "," ++ String.fromFloat mixedColor.green ++ "," ++ String.fromFloat mixedColor.blue ++ ")"


type alias RGB =
    { red : Float
    , green : Float
    , blue : Float
    }


mixColors : RGB -> RGB -> Float -> RGB
mixColors rgb1 rgb2 weight =
    { red = (rgb1.red * weight) + (rgb2.red * 1 - weight)
    , green = (rgb1.green * weight) + (rgb2.green * 1 - weight)
    , blue = (rgb1.blue * weight) + (rgb2.blue * 1 - weight)
    }
