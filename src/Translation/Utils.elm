module Translation.Utils
    exposing
        ( Language(..)
        , TranslationId(..)
        , translate
        )


type alias TranslationSet =
    { finnish : String
    , english : String
    }


type TranslationId
    = FlexBalance
    | MonthFlexBalance
    | InputPreviousBalance
    | CloseButton


type Language
    = English
    | Finnish


translate : Language -> TranslationId -> String
translate lang trans =
    let
        translationSet =
            case trans of
                FlexBalance ->
                    TranslationSet "Tuntisaldo:" "Flex balance:"

                MonthFlexBalance ->
                    TranslationSet "Kuukauden tuntisaldo:" "Flex balance for current month:"

                InputPreviousBalance ->
                    TranslationSet "Aseta vanha saldo:" "Input starting flex balance:"

                CloseButton ->
                    TranslationSet "Sulje" "Close"
    in
    case lang of
        Finnish ->
            .finnish translationSet

        English ->
            .english translationSet
