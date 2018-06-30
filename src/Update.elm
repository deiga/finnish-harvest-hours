module Update exposing (..)

import Api exposing (getEntries)
import Data.Model exposing (..)
import Data.User exposing (..)
import Date exposing (fromTime)
import Date.Extra.Duration as Duration
import DateUtils exposing (calculateHourBalance, hourBalanceOfCurrentMonth)
import Http exposing (Response)
import List exposing (isEmpty)
import Material
import Navigation exposing (load)
import String
import Task exposing (Task)
import Time
import Translation.Utils exposing (Language)


type Msg
    = Login
    | GetDayEntries
    | EntryList (Result Http.Error (List DateEntries))
    | FetchedUser (Result Http.Error User)
    | FetchedHolidays (Result Http.Error Holidays)
    | UpdateHours
    | PreviousMonth
    | NextMonth
    | UpdateHourBalanceOfCurrentMonth
    | FetchedSpecialTaskList (Result Http.Error SpecialTasks)
    | SetCurrentTime Time.Time
    | UpdatePreviousBalance String
    | SavePreviousBalance Float
    | PreviousBalanceSaved (Result Http.Error String)
    | NavigateTo String
    | Mdl (Material.Msg Msg)
    | SetLanguage Language
    | SetCurrentCity City
    | CurrentCitySaved (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Login ->
            noFx model

        GetDayEntries ->
            ( model, getEntries )

        EntryList results ->
            case results of
                Ok entries ->
                    update UpdateHours { model | entries = entries }

                Err error ->
                    handleError model error

        FetchedUser result ->
            case result of
                Ok user ->
                    update UpdateHours
                        { model
                            | user = user
                            , previousBalanceString = toString user.previousBalance
                            , previousBalance = user.previousBalance
                        }

                Err error ->
                    handleError model error

        FetchedHolidays result ->
            case result of
                Ok holidays ->
                    update UpdateHours { model | holidays = holidays }

                Err error ->
                    handleError model error

        UpdateHours ->
            if
                not
                    (isEmpty model.entries
                        || isEmpty model.holidays
                        || isEmpty model.specialTasks.ignore
                    )
            then
                let
                    newModel =
                        { model | loading = False }

                    hourBalance =
                        calculateHourBalance model
                in
                update UpdateHourBalanceOfCurrentMonth
                    { newModel
                        | totalHours = Just hourBalance.normalHours
                        , kikyHours = Just hourBalance.kikyHours
                    }
            else
                noFx model

        PreviousMonth ->
            update UpdateHourBalanceOfCurrentMonth { model | currentDate = Duration.add Duration.Month -1 model.currentDate }

        NextMonth ->
            update UpdateHourBalanceOfCurrentMonth { model | currentDate = Duration.add Duration.Month 1 model.currentDate }

        UpdateHourBalanceOfCurrentMonth ->
            noFx { model | hourBalanceOfCurrentMonth = Just (hourBalanceOfCurrentMonth model) }

        FetchedSpecialTaskList result ->
            case result of
                Ok tasks ->
                    update UpdateHours { model | specialTasks = tasks }

                Err error ->
                    handleError model error

        SetCurrentTime currentTime ->
            noFx { model | currentDate = Date.fromTime currentTime, today = Date.fromTime currentTime }

        UpdatePreviousBalance balance ->
            updatePreviousBalance model balance

        SavePreviousBalance balance ->
            ( model, setPreviousBalance balance )

        PreviousBalanceSaved (Ok result) ->
            update UpdateHours model

        PreviousBalanceSaved (Err err) ->
            let
                log =
                    Debug.log "Error saving balance:" err
            in
            noFx model

        NavigateTo url ->
            ( model, Navigation.load url )

        Mdl action_ ->
            Material.update Mdl action_ model

        SetLanguage lang ->
            noFx { model | currentLanguage = lang }

        SetCurrentCity city ->
            let
                oldUser = model.user
                newUser = { oldUser | currentCity = city }
            in
            ( { model | user = newUser }, setCurrentCity city )

        CurrentCitySaved (Ok result) ->
            update UpdateHours model

        CurrentCitySaved (Err err) ->
            let
                log =
                    Debug.log "Error saving city:" err
            in
            noFx model


updatePreviousBalance : Model -> String -> ( Model, Cmd Msg )
updatePreviousBalance model balance =
    case String.toFloat balance of
        Err error ->
            noFx { model | previousBalanceString = balance }

        Ok value ->
            noFx { model | previousBalance = value, previousBalanceString = balance }


setPreviousBalance : Float -> Cmd Msg
setPreviousBalance balance =
    Http.send PreviousBalanceSaved (Api.setPreviousBalance balance)


setCurrentCity : City -> Cmd Msg
setCurrentCity city =
    Http.send CurrentCitySaved (Api.setCurrentCity city)


currentTime : Cmd Msg
currentTime =
    Task.perform SetCurrentTime Time.now


noFx : Model -> ( Model, Cmd Msg )
noFx model =
    ( model, Cmd.none )


handleError : Model -> Http.Error -> ( Model, Cmd Msg )
handleError model error =
    case error of
        Http.BadStatus response ->
            let
                newModel =
                    { model | loading = False }
            in
            case response.status.code of
                401 ->
                    update (NavigateTo "/login") newModel

                _ ->
                    noFx { newModel | httpError = Err error }

        _ ->
            noFx { model | httpError = Err error }


getEntries : Cmd Msg
getEntries =
    Http.send EntryList Api.getEntries


getUser : Cmd Msg
getUser =
    Http.send FetchedUser Api.getUser


getHolidays : Cmd Msg
getHolidays =
    Http.send FetchedHolidays Api.getNationalHolidays


getSpecialTasks : Cmd Msg
getSpecialTasks =
    Http.send FetchedSpecialTaskList Api.getSpecialTasks
