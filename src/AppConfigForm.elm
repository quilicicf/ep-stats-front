module AppConfigForm
  exposing
  ( AppConfig
  , main
--  ,decodeKey
  )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as Decode exposing (Decoder, decodeString, bool, int, list, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode

-- DECLARATIONS

type alias AppConfig
  = { teamName: String
    , sheetId: String
    , apiKey: String
    }

--appConfigDecoder : Decoder AppConfig
--appConfigDecoder =
--  Decode.succeed AppConfig
--    |> required "teamName" string
--    |> required "sheetId" string
--    |> required "apiKey" string

--decodeKey : Key -> AppConfig
--decodeKey key = decodeString appConfigDecoder key

-- MAIN

main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }

type alias Model =
  { teamName: String
  , sheetId: String
  , apiKey: String
  }

-- INIT

init : Model
init = Model "" "" ""

--UPDATE

type Msg
  = UpdateTeamName String
  | UpdateSheetId String
  | UpdateApiKey String

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTeamName teamName -> { model | teamName = teamName }

    UpdateSheetId sheetId -> { model | sheetId = sheetId }

    UpdateApiKey apiKey -> { model | apiKey = apiKey }

-- VIEW

view : Model -> Html Msg
view model =
  div []
      [ viewInput "text" "Name" model.teamName UpdateTeamName
      , viewInput "password" "Password" model.sheetId UpdateSheetId
      , viewInput "password" "Re-enter Password" model.apiKey UpdateApiKey
      ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput inputType placeHolder inputValue toMsg =
  input [ type_ inputType, placeholder placeHolder, value inputValue, onInput toMsg ] []
