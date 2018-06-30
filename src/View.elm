module View exposing (..)

import Material.Dialog as Dialog
import Material.Button as Button
import Material.Options as Options
import Round
import List
import Data.Model exposing (..)
import Data.User exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import DateUtils exposing (..)
import Calendar exposing (monthView)
import Date exposing (..)
import String
import Update exposing (..)
import Formatting exposing (floatToHoursAndMins)
import Translation.Utils exposing (..)

view : Model -> Html Msg
view model =
    case model.httpError of
        Err err ->
            div [ style [ ( "color", "red" ) ] ]
                [ text (toString err) ]

        Ok _ ->
            div [ class "main" ]
                [ viewLanguageSwitcher model
                , userSettingsDialog model
                , balanceHeader model
                , displayKiKyHours model
                , navigationPane model
                , calendarTable model
                ]


displayKiKyHours : Model -> Html Msg
displayKiKyHours model =
    if model.currentLanguage == Finnish then
        div [ class "kiky" ]
            [ text (String.join " " [ "Kikytunnit:", floatToHoursAndMins model.kikyHours ]) ]
    else
        text ""


balanceHeader : Model -> Html Msg
balanceHeader model =
    div [ class "header" ]
        [ span [ class "name" ]
            [ text (getFullName model.user) ]
        , Button.render Mdl
            [ 1 ]
            model.mdl
            [ Dialog.openOn "click"
            , Options.cs "calendar-button"
            ]
            [ i [ class "fa settings fa-cog" ] [] ]
        , text (String.join " " [ translate model.currentLanguage FlexBalance, floatToHoursAndMins model.totalHours ])
        ]


viewLanguageSwitcher : Model -> Html Msg
viewLanguageSwitcher model =
  let
    -- Check if a language is the current language
    isCurrent lang =
      model.currentLanguage == lang

    button_ lang name =
      button
        [ disabled (isCurrent lang)
        , onClick <| SetLanguage lang
        ]
        [ text name ]

  in
    div
      [ languageSwitcherStyle ]
      [ button_ English "English"
      , button_ Finnish "Finnish"
      ]

languageSwitcherStyle : Attribute msg
languageSwitcherStyle =
  style
    [
      ("position", "fixed")
      , ("top", "1vh")
      , ("right", "1vw")
    ]




userSettingsDialog : Model -> Html Msg
userSettingsDialog model =
    Dialog.view []
        [ Dialog.title [] [ h3 [] [ text <| translate model.currentLanguage InputPreviousBalance ] ]
        , Dialog.content []
            [ input
                [ class "balance-input"
                , onInput UpdatePreviousBalance
                , onBlur (SavePreviousBalance model.previousBalance)
                , value model.previousBalanceString
                ]
                []
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.cs "close-button"
                ]
                [ text <| translate model.currentLanguage CloseButton ]
            ]
        ]


navigationPane : Model -> Html Msg
navigationPane model =
    div [ class "navigation" ]
        [ div []
            [ button [ onClick PreviousMonth, class "nav-button float-left" ]
                [ i [ class "fa fa-arrow-left" ] [] ]
            ]
        , div []
            [ button [ onClick NextMonth, class "nav-button float-left" ]
                [ i [ class "fa fa-arrow-right" ] [] ]
            ]
        , div [ class "monthly-balance float-left" ]
            [ text
                (String.join " "
                    [ translate model.currentLanguage MonthFlexBalance
                    , floatToHoursAndMins model.hourBalanceOfCurrentMonth
                    ]
                )
            ]
        , div [ class "spinner" ] [ i [ class (spinnerClass model) ] [] ]
        ]


spinnerClass : Model -> String
spinnerClass model =
    if model.loading then
        "fa fa-spinner fa-pulse spinner"
    else
        ""


calendarTable : Model -> Html Msg
calendarTable model =
    table [ class "month-view-table" ]
        [ thead []
            [ tr []
                []
            ]
        , tbody []
            (List.map (\week -> weekRow model week)
                (monthView model)
            )
        ]


weekRow : Model -> List DateEntries -> Html Msg
weekRow model dateList =
    tr []
        (List.map
            (\dateEntries ->
                td [ class (dayCellClass model dateEntries) ]
                    [ div [] [ text (dateFormat dateEntries.date) ]
                    , div [ class "hours" ]
                        [ text (hourString (.normalHours (calculateDailyHours dateEntries model)))
                        ]
                    ]
            )
            dateList
        )


hourString : Float -> String
hourString hours =
    if hours == 0 then
        ""
    else
        floatToHoursAndMins (Just hours)


dayCellClass : Model -> DateEntries -> String
dayCellClass model dateEntries =
    if not (isWorkDay dateEntries.date model.holidays) then
        "day-off"
    else if dayHasOnlySpecialTasks dateEntries model.specialTasks then
        "special-day"
    else if month dateEntries.date == month model.currentDate then
        "current-month"
    else
        "other-month"
