module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import FormattingTest
import DateUtilsTest
import Translation.Test


all : Test
all =
    describe
        "Test Suite"
        [ describe "Unit tests"
            [ FormattingTest.all
            , DateUtilsTest.all
            , Translation.Test.all
            ]
        ]
