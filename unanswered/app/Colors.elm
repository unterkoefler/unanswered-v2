module Colors exposing (ColorScheme(..), accent, accentDark, disabled, link, neutral, primary, secondary, white)

import Element exposing (Color, rgb255)


type ColorScheme
    = Light
    | Dark


primary : ColorScheme -> Color
primary =
    ldSwitch black white


secondary : ColorScheme -> Color
secondary =
    ldSwitch white black


accent : ColorScheme -> Color
accent _ =
    teal


link : ColorScheme -> Color
link =
    ldSwitch darkTeal teal


neutral : ColorScheme -> Color
neutral _ =
    gray


disabled : ColorScheme -> Color
disabled _ =
    gray


accentDark : ColorScheme -> Color
accentDark _ =
    darkTeal


ldSwitch : Color -> Color -> ColorScheme -> Color
ldSwitch lightModeColor darkModeColor colorScheme =
    case colorScheme of
        Light ->
            lightModeColor

        Dark ->
            darkModeColor


teal =
    rgb255 103 201 207


gray =
    rgb255 80 80 80


darkTeal =
    rgb255 64 124 128


white =
    rgb255 255 255 255


black =
    rgb255 0 0 0
