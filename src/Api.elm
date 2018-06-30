module Api exposing (..)

import Data.Model exposing (..)
import Data.User exposing (..)
import Http exposing (Body, Error, Request, expectString, jsonBody)
import Json.Decode as Json exposing (..)
import Json.Decode.Extra exposing (date)
import Json.Encode as Encode


getUser : Request User
getUser =
    Http.get "/user" decodeUser


decodeUser : Json.Decoder User
decodeUser =
    map4 User
        (field "firstName" string)
        (field "lastName" string)
        (field "previousBalance" float)
        (field "currentCity" cityDecoder)


getEntries : Request (List DateEntries)
getEntries =
    Http.get "/entries" decodeDayEntries


decodeDayEntries : Json.Decoder (List DateEntries)
decodeDayEntries =
    list
        (map2 DateEntries
            (field "date" date)
            (field "entries" (list decodeEntry))
        )


decodeEntry : Json.Decoder Entry
decodeEntry =
    map2 Entry
        (field "hours" float)
        (field "taskId" int)


getNationalHolidays : Request (List ( City, List Holiday ))
getNationalHolidays =
    Http.get "/holidays" decodeHolidays


decodeHolidays : Json.Decoder (List ( City, List Holiday ))
decodeHolidays =
    list
        (map2 (,)
            (field "city" cityDecoder)
            (field "holidays" decodeHolidayList)
        )


cityDecoder : Json.Decoder City
cityDecoder =
    string
        |> andThen
            (\str ->
                case str of
                    "Helsinki" ->
                        succeed Helsinki

                    "Berlin" ->
                        succeed Berlin

                    "Lund" ->
                        succeed Lund

                    somethingElse ->
                        fail <| "Unknown city: " ++ somethingElse
            )


decodeHolidayList : Json.Decoder (List Holiday)
decodeHolidayList =
    list
        (map2 Holiday
            (field "date" date)
            (field "name" string)
        )


getSpecialTasks : Request SpecialTasks
getSpecialTasks =
    Http.get "/special_tasks" decodeTasks


decodeTasks : Json.Decoder SpecialTasks
decodeTasks =
    map2 SpecialTasks
        (field "ignore"
            (list
                (map HarvestTask
                    (field "taskId" int)
                )
            )
        )
        (field "kiky"
            (list
                (map HarvestTask
                    (field "taskId" int)
                )
            )
        )


setPreviousBalance : Float -> Request String
setPreviousBalance balance =
    httpPost "/balance"
        (jsonBody
            (Encode.object
                [ ( "balance", Encode.float balance )
                ]
            )
        )


httpPost : String -> Body -> Request String
httpPost url body =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , expect = expectString
        , timeout = Nothing
        , withCredentials = False
        }
