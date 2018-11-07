module Formatting exposing (floatToHoursAndMins)

import Round as R exposing (round)


{-| Take a float and return a string with hours and minutes, eg.
4.25 -> "4:15"
-}
floatToHoursAndMins : Maybe Float -> String
floatToHoursAndMins hours =
    case hours of
        Nothing ->
            ""

        Just val ->
            let
                hoursAndDecimals =
                    R.round 2 val |> String.split "."
            in
            case hoursAndDecimals of
                [ hourString, decimalString ] ->
                    let
                        minString =
                            Result.withDefault 0 (Result.fromMaybe "Error parsing decimal to Float" (String.toFloat decimalString))
                                * 0.6
                                |> R.round 0
                                |> String.padRight 2 '0'
                    in
                    String.join ":" [ hourString, minString ]

                _ ->
                    ""
