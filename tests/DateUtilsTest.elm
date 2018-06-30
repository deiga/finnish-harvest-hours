module DateUtilsTest exposing (all)

import Data.Model exposing (..)
import Date exposing (Date, Month(..))
import Date.Extra.Create exposing (dateFromFields)
import DateUtils exposing (..)
import Expect
import Test exposing (..)


all : Test
all =
    describe "Date utils"
        [ test "only entries within current month are added up" <|
            \() ->
                let
                    model =
                        { initialModel
                            | currentDate = dateFromFields 2017 Mar 5 0 1 0 0
                            , hoursInWorkDay = 7.5
                            , today = dateFromFields 2017 Mar 2 0 1 0 0
                            , entries =
                                [ DateEntries (dateFromFields 2017 Feb 28 22 59 0 0)
                                    [ Entry 2.5 123, Entry 7 234 ]
                                , DateEntries (dateFromFields 2017 Mar 1 22 59 0 0)
                                    [ Entry 5 123, Entry 2.5 234 ]
                                , DateEntries (dateFromFields 2017 Mar 2 22 59 0 0)
                                    [ Entry 2.5 123, Entry 5 234 ]
                                , DateEntries (dateFromFields 2017 Apr 1 0 0 0 0)
                                    [ Entry 2.5 123, Entry 7 234 ]
                                ]
                        }
                in
                hourBalanceOfCurrentMonth model
                    |> Expect.equal 0
        , test "day has only special task entries" <|
            \() ->
                let
                    specialTasks =
                        { kiky = [ { id = 123 } ]
                        , ignore = [ { id = 234 } ]
                        }

                    dateEntries =
                        DateEntries (dateFromFields 2017 Feb 28 0 0 0 0)
                            [ Entry 2.5 123, Entry 7 234 ]
                in
                dayHasOnlySpecialTasks dateEntries specialTasks
                    |> Expect.true "Expected the day to have only special task entries."
        , test "day has no task entries" <|
            \() ->
                let
                    specialTasks =
                        { kiky = [ { id = 123 } ]
                        , ignore = [ { id = 234 } ]
                        }

                    dateEntries =
                        DateEntries (dateFromFields 2017 Feb 28 0 0 0 0)
                            []
                in
                dayHasOnlySpecialTasks dateEntries specialTasks
                    |> Expect.false "Expected the day to have no special task entries."
        , test "day has special and normal task entries" <|
            \() ->
                let
                    specialTasks =
                        { kiky = [ { id = 123 } ]
                        , ignore = [ { id = 234 } ]
                        }

                    dateEntries =
                        DateEntries (dateFromFields 2017 Feb 28 0 0 0 0)
                            [ Entry 2.5 123, Entry 2.5 234, Entry 2 1000 ]
                in
                dayHasOnlySpecialTasks dateEntries specialTasks
                    |> Expect.false "Expected the day to have also normal task entries."
        ]
