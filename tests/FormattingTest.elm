module FormattingTest exposing (all)

import Expect
import Formatting exposing (floatToHoursAndMins)
import Round exposing (round)
import Test exposing (..)


all : Test
all =
    describe "Hour formatting"
        [ test "Format Int to String with two decimals" <|
            \() ->
                Round.round 2 5
                    |> Expect.equal "5.00"
        , test "Format int to hours and minutes" <|
            \() ->
                floatToHoursAndMins (Just 4) |> Expect.equal "4:00"
        , test "Format 4.25" <|
            \() ->
                floatToHoursAndMins (Just 4.25) |> Expect.equal "4:15"
        , test "Format 0.333" <|
            \() ->
                floatToHoursAndMins (Just 0.333) |> Expect.equal "0:20"
        , test "Format 0.21" <|
            \() ->
                floatToHoursAndMins (Just 0.21) |> Expect.equal "0:13"
        ]
