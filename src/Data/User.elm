module Data.User exposing (..)

type alias User =
    { firstName : String
    , lastName : String
    , previousBalance : Float
    , currentCity : City
    }

type City = Helsinki | Berlin | Lund

getFullName : User -> String
getFullName user = String.join " " [ user.firstName, user.lastName]
