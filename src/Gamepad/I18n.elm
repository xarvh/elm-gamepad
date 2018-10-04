module Gamepad.I18n exposing (Lang(..), Translations, getLocale)


type alias Translations =
    { noGamepadsDetected : String
    , remappingGamepadComplete : Int -> String
    , pressAnyButtonToGoBack : String
    , remappingGamepad : Int -> String
    , press : String
    , skipThisAction : String
    , cancelRemapping : String
    , map : String
    , needsMapping : String
    , idle : String
    , receivingSignal : String
    , remap : String
    , standardMapping : String
    , customMapping : String
    }


type Lang
    = En
    | Fr


getLocale : Lang -> Translations
getLocale lang =
    case lang of
        En ->
            { noGamepadsDetected = "No gamepads detected"
            , remappingGamepadComplete = \id -> "Remapping Gamepad " ++ String.fromInt id ++ " complete."
            , pressAnyButtonToGoBack = "Press any button to go back."
            , remappingGamepad = \id -> "Remapping Gamepad " ++ String.fromInt id
            , press = "Press:"
            , skipThisAction = "Skip this action"
            , cancelRemapping = "Cancel remapping"
            , map = "Map"
            , needsMapping = "Needs mapping"
            , idle = "idle"
            , receivingSignal = "Receiving signal"
            , remap = "Remap"
            , standardMapping = "Standard mapping"
            , customMapping = "Custom mapping"
            }

        Fr ->
            { noGamepadsDetected = "Aucune manette détectée"
            , remappingGamepadComplete = \id -> "Configuration de la manette " ++ String.fromInt id ++ " terminée."
            , pressAnyButtonToGoBack = "Pressez n'importe quelle touche pour revenir en arrière."
            , remappingGamepad = \id -> "Configuration de la manette " ++ String.fromInt id
            , press = "Pressez :"
            , skipThisAction = "Passer cette action"
            , cancelRemapping = "Annuler la configuration"
            , map = "Attribuer"
            , needsMapping = "Configuration nécessaire"
            , idle = "inactif"
            , receivingSignal = "Réception d'un signal"
            , remap = "Configurer"
            , standardMapping = "Configuration standard"
            , customMapping = "Configuration personnalisée"
            }
