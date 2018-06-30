module Main exposing (..)

import Data.Model exposing (..)
import Data.User exposing (..)
import Date exposing (..)
import Html
import Material
import Translation.Utils exposing (..)
import Update exposing (Msg, update)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.batch
        [ Update.currentTime
        , Update.getUser
        , Update.getEntries
        , Update.getHolidays
        , Update.getSpecialTasks
        ]
    )


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
