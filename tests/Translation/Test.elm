module Translation.Test exposing (all)

import Test exposing (..)
import Expect

import Translation.Utils exposing (..)

all : Test
all =
  describe "Translation tests"
    [ test "English no arguments" <|
      \() ->
        translate English FlexBalance
          |> Expect.equal "Flex balance:"
    , test "Finnish no arguments" <|
      \() ->
        translate Finnish FlexBalance
          |> Expect.equal "Tuntisaldo:"
    ]
