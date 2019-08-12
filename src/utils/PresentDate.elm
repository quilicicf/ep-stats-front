module PresentDate exposing (presentDate)

presentDate : String -> String
presentDate date = String.replace "_" "/" date
