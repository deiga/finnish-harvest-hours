module Calendar exposing (..)

import Data.Model exposing (..)
import Date exposing (..)
import Date.Extra.Compare as Compare exposing (Compare2, Compare3, is)
import Date.Extra.Core exposing (..)
import Date.Extra.Period as Period exposing (add, diff)
import DateUtils exposing (..)
import List exposing (drop, head, isEmpty, reverse, take)


{-| Set up calendar table data.
-}
monthView : Model -> List (List DateEntries)
monthView model =
    weekRows (monthDays model) []


weekRows : List DateEntries -> List (List DateEntries) -> List (List DateEntries)
weekRows entryList result =
    if isEmpty entryList then
        reverse result
    else
        weekRows (drop 7 entryList) (take 7 entryList :: result)


monthDays : Model -> List DateEntries
monthDays model =
    dateRange model
        (add Period.Day -(firstOfMonthDayOfWeek model) (toFirstOfMonth model.currentDate))
        (lastOfMonthDate model.currentDate)
        []


{-| Build a list of days with sum of entered hours.
Set hour at 3 hours past midnight to avoid DST problems.
-}
dateRange : Model -> Date -> Date -> List DateEntries -> List DateEntries
dateRange model startDate endDate dateList =
    if Compare.is Compare.After startDate endDate then
        reverse dateList
    else
        let
            mDateEntries =
                singleDaysEntries model startDate

            dateEntries =
                case mDateEntries of
                    Nothing ->
                        { date = startDate, entries = [] }

                    Just val ->
                        val

            nextDay =
                add Period.Hour 3 (add Period.Day 1 (startOfDate startDate))
        in
        dateRange model
            nextDay
            endDate
            (dateEntries :: dateList)


singleDaysEntries : Model -> Date -> Maybe DateEntries
singleDaysEntries model date =
    List.head
        (List.filter (\dateEntries -> isSameDate date dateEntries.date)
            model.entries
        )


{-| Total entered hours for a date.
-}
sumDateHours : Model -> Date -> DateHours
sumDateHours model date =
    let
        dateEntries =
            List.head
                (List.filter (\dateEntries -> isSameDate date dateEntries.date)
                    model.entries
                )
    in
    case dateEntries of
        Nothing ->
            { date = date
            , normalHours = 0
            , kikyHours = 0
            }

        Just entries ->
            calculateDailyHours entries model


{-| Day of week of the first day of the month as Int, from 0 (Mon) to 6 (Sun).
-}
firstOfMonthDayOfWeek : Model -> Int
firstOfMonthDayOfWeek model =
    isoDayOfWeek (dayOfWeek (toFirstOfMonth model.currentDate)) - 1
