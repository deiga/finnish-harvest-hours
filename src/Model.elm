module Model exposing (..)

import Material
import Date exposing (Date, Month)
import Http
import Translation.Utils exposing (Language)


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
    , holidays : List Holiday
    , specialTasks : SpecialTasks
    , hoursInWorkDay : Float
    , previousBalanceString : String
    , previousBalance : Float
    , mdl : Material.Model
    , currentLanguage : Language
    }


type alias User =
    { firstName : String
    , lastName : String
    , previousBalance : Float
    }


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
