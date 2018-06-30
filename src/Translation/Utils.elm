module Translation.Utils
    exposing
        ( Language(..)
        , TranslationId(..)
        , translate
        )


type alias TranslationSet =
    { finnish : String
    , english : String
    , german : String
    , swedish : String
    }


type TranslationId
    = FlexBalance
    | MonthFlexBalance
    | InputPreviousBalance
    | CloseButton
    | UserSettings
    | CurrentCity


type Language
    = English
    | Finnish
    | German
    | Swedish


translate : Language -> TranslationId -> String
translate lang trans =
    let
        translationSet =
            case trans of
                FlexBalance ->
                    TranslationSet "Tuntisaldo:" "Flex balance:" "" ""

                MonthFlexBalance ->
                    TranslationSet "Kuukauden tuntisaldo:" "Flex balance for current month:" "" ""

                InputPreviousBalance ->
                    TranslationSet "Aseta vanha saldo:" "Input starting flex balance:" "" ""

                CloseButton ->
                    TranslationSet "Sulje" "Close" "" ""

                UserSettings ->
                    TranslationSet "Käyttäjän Asetukset" "User Settings" "" ""

                CurrentCity ->
                    TranslationSet "Missä Kaupungissa olet Töissä?" "Which City do you work in?" "" ""
    in
    case lang of
        Finnish ->
            .finnish translationSet

        English ->
            .english translationSet

        German ->
            .english translationSet

        Swedish ->
            .english translationSet
