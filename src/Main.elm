module Main exposing (..)

import Data.Model exposing (..)
import Data.User exposing (..)
import Date exposing (..)
import Html
import Material
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
