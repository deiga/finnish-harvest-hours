module Data.Model exposing (..)

import Data.User exposing (..)
import Date exposing (Date, Month)
import Http
import Material
import Translation.Utils exposing (..)


type alias Model =
    { httpError : Result Http.Error ()
    , loading : Bool
    , today : Date
    , currentDate : Date
    , entries : List DateEntries
    , totalHours : Maybe Float
    , kikyHours : Maybe Float
    , hourBalanceOfCurrentMonth : Maybe Float
    , user : User
    , holidays : Holidays
    , specialTasks : SpecialTasks
    , hoursInWorkDay : Float
    , previousBalanceString : String
    , previousBalance : Float
    , mdl : Material.Model
    , currentLanguage : Language
    }


getUserHolidays : City -> Holidays -> List Holiday
getUserHolidays currentCity holidays =
    let
        matchCurrentCity =
            \x -> Tuple.first x == currentCity

        currentCityMatches =
            List.filter matchCurrentCity holidays

        firstMatch =
            List.head currentCityMatches
    in
    case firstMatch of
        Nothing ->
            []

        Just match ->
            Tuple.second match


type alias Holidays =
    List ( City, List Holiday )


type alias DateEntries =
    { date : Date
    , entries : List Entry
    }


type alias Entry =
    { hours : Float
    , taskId : Int
    }


type alias Holiday =
    { date : Date
    , name : String
    }


type alias HarvestTask =
    { id : Int }


type alias SpecialTasks =
    { ignore : List HarvestTask
    , kiky : List HarvestTask
    }


type alias Hours a =
    { a
        | normalHours : Float
        , kikyHours : Float
    }


type alias DateHours =
    Hours { date : Date }


initialModel : Model
initialModel =
    { httpError = Ok ()
    , loading = True
    , today = Date.fromTime 0
    , currentDate = Date.fromTime 0
    , entries = []
    , totalHours = Nothing
    , kikyHours = Nothing
    , hourBalanceOfCurrentMonth = Nothing
    , user = { firstName = "", lastName = "", previousBalance = 0, currentCity = Berlin }
    , holidays = []
    , specialTasks =
        { ignore = []
        , kiky = []
        }
    , hoursInWorkDay = 8
    , previousBalanceString = ""
    , previousBalance = 0
    , mdl = Material.model
    , currentLanguage = English
    }
