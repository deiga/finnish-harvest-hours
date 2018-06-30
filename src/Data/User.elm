module Data.User exposing (..)

import Json.Decode as Decode exposing (Decoder, andThen, fail, string, succeed, map4, field, float)
import Json.Encode as Encode exposing (Value, string)


type alias User =
    { firstName : String
    , lastName : String
    , previousBalance : Float
    , currentCity : City
    }


type City
    = Helsinki
    | Berlin
    | Lund


getFullName : User -> String
getFullName user =
    String.join " " [ user.firstName, user.lastName ]


decodeUser : Decoder User
decodeUser =
    map4 User
        (field "firstName" Decode.string)
        (field "lastName" Decode.string)
        (field "previousBalance" float)
        (field "currentCity" decodeCity)


encodeCity : City -> Value
encodeCity city =
    case city of
        Helsinki ->
            Encode.string "Helsinki"

        Berlin ->
            Encode.string "Berlin"

        Lund ->
            Encode.string "Lund"


decodeCity : Decoder City
decodeCity =
    Decode.string
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
