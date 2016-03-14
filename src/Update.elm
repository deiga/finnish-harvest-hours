module Update (..) where

import List exposing (isEmpty)
import Effects exposing (Effects)
import Task exposing (Task)
import Http
import Model exposing (..)
import Api exposing (getEntries)
import DateUtils exposing (enteredHoursVsTotal)
import Date.Duration as Duration


type Action
  = Login
  | GetDayEntries
  | EntryList (Result Http.Error (List DateEntries))
  | FetchedUser (Result Http.Error (User))
  | FetchedHolidays (Result Http.Error (List Holiday))
  | UpdateHours
  | PreviousMonth
  | NextMonth
  | FetchedAbsenceTaskList (Result Http.Error (List HarvestTask))


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Login ->
      noFx model

    GetDayEntries ->
      ( model, getEntries )

    EntryList results ->
      let
        newModel =
          { model | loading = False }
      in
        case results of
          Ok entries ->
            update UpdateHours { newModel | entries = entries }

          Err error ->
            handleError newModel error

    FetchedUser result ->
      case result of
        Ok user ->
          noFx { model | user = user }

        Err error ->
          handleError model error

    FetchedHolidays result ->
      case result of
        Ok holidays ->
          update UpdateHours { model | holidays = holidays }

        Err error ->
          handleError model error

    UpdateHours ->
      if not (isEmpty model.entries || isEmpty model.holidays) then
        noFx { model | totalHours = enteredHoursVsTotal model }
      else
        noFx model

    PreviousMonth ->
      noFx { model | currentDate = Duration.add Duration.Month -1 model.currentDate }

    NextMonth ->
      noFx { model | currentDate = Duration.add Duration.Month 1 model.currentDate }

    FetchedAbsenceTaskList result ->
      case result of
        Ok tasks ->
          noFx { model | absenceTasks = tasks }

        Err error ->
          handleError model error


noFx : Model -> ( Model, Effects Action )
noFx model =
  ( model, Effects.none )


handleError : Model -> Http.Error -> ( Model, Effects Action )
handleError model error =
  noFx { model | httpError = Err error }


getResult : Task Http.Error a -> (Result Http.Error a -> Action) -> Effects Action
getResult httpGet action =
  httpGet
    |> Task.toResult
    |> Task.map action
    |> Effects.task


getEntries : Effects Action
getEntries =
  getResult Api.getEntries EntryList


getUser : Effects Action
getUser =
  getResult Api.getUser FetchedUser


getHolidays : Effects Action
getHolidays =
  getResult Api.getNationalHolidays FetchedHolidays


getAbsenceTasks : Effects Action
getAbsenceTasks =
  getResult Api.getAbsenceTasks FetchedAbsenceTaskList
