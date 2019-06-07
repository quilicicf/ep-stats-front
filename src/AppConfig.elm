module AppConfig exposing (..)

import Base64

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as Decode exposing(Value, Decoder, string, bool)
import Json.Encode as Encode

import Json.Decode.Pipeline exposing (required, optional)

import Msg exposing (..)

------------
-- MODELS --
------------

type alias AppConfig =
  { teamName: String
  , sheetId: String
  , apiKey: String
  , isAdmin: Bool
  }

type alias AppConfigExtender r =
  { r
  | teamName: String
  , sheetId: String
  , apiKey: String
  , isAdmin: Bool
  , appKey: String
  , appKeyError: String
  }

type alias StorageAppState = { appKey: String }

-----------
-- UTILS --
-----------

jsonifyAppConfig : AppConfigExtender r -> Bool -> Value
jsonifyAppConfig appConfig isAdmin =
  Encode.object
      [ ("teamName", Encode.string appConfig.teamName)
      , ("sheetId", Encode.string appConfig.sheetId)
      , ("apiKey", Encode.string appConfig.apiKey)
      , ("isAdmin", Encode.bool isAdmin)
      ]

encodeAppConfig : AppConfigExtender r -> Bool -> String
encodeAppConfig appConfig isAdmin =
  jsonifyAppConfig appConfig isAdmin
    |> Encode.encode 0
    |> Base64.encode

appConfigDecoder : Decoder AppConfig
appConfigDecoder =
  Decode.succeed AppConfig
    |> required "teamName" string
    |> required "sheetId" string
    |> required "apiKey" string
    |> required "isAdmin" bool

storageAppStateDecoder : Decoder StorageAppState
storageAppStateDecoder =
  Decode.succeed StorageAppState
    |> optional "appKey" string ""

decodeStorageAppState : Value -> StorageAppState
decodeStorageAppState appKeyAsJson =
  case Decode.decodeValue storageAppStateDecoder appKeyAsJson of
    Ok appState -> appState
    Err _ -> StorageAppState ""

decodeAppConfigFromAppKey : String -> Maybe AppConfig
decodeAppConfigFromAppKey appKeyInBase64 =
  case Base64.decode appKeyInBase64 of
    Ok decodedAppKey ->
      case Decode.decodeString appConfigDecoder decodedAppKey of
        Ok appConfig -> Just appConfig
        Err _ -> Nothing
    Err _ -> Nothing

----------
-- VIEW --
----------

viewAppConfig : AppConfigExtender r -> Html Msg
viewAppConfig appConfig =
  Html.form []
    [ div []
        [ text "Team name"
        , br [] []
        , input
            [ type_ "text"
            , value appConfig.teamName
            , onInput (AppConfigMsg << NewTeamName)
            ]
            []
        ]
    , br [] []
    , div []
        [ text "Sheet id"
        , br [] []
        , input
            [ type_ "text"
            , value appConfig.sheetId
            , onInput (AppConfigMsg << NewSheetId)
            ]
            []
        ]
    , br [] []
    , div []
        [ text "API key"
        , br [] []
        , input
            [ type_ "text"
            , value appConfig.apiKey
            , onInput (AppConfigMsg << NewApiKey)
            ]
            []
        ]
    , br [] []
    , div []
        [ button
            [ type_ "button", onClick (AppConfigMsg CreateAppConfig) ]
            [ text "Create" ]
        ]
    ]

viewAppKeyCopier : AppConfigExtender r -> Html Msg
viewAppKeyCopier appConfig =
  let
    adminAppKey : String
    adminAppKey = encodeAppConfig appConfig True

    peonAppKey : String
    peonAppKey = encodeAppConfig appConfig False

  in
    div [] [
      h2 [] [ text "Copy the keys and validate" ],
      div [] [
        span [] [ text "Admin key" ],
        pre [] [ text adminAppKey ]
      ],
      div [] [
        span [] [ text "Peon key" ],
        pre [] [ text peonAppKey ]
      ],
      button
        [ type_ "button", onClick (AppConfigMsg (CopiedAppKeys adminAppKey)) ]
        [ text "I've stored'em away" ]
    ]

viewAppKeyInput : AppConfigExtender r -> Html Msg
viewAppKeyInput { appKeyError } =
  Html.form [ class "input-app-key" ]
    [ h2 [] [ text "Paste your app key" ]
    , div [ class "form-field" ]
        [ label [ for "appKey" ] [ text "App key" ]
        , input
            [ type_ "text"
            , id "appKey"
            , onInput (AppConfigMsg << NewAppKey)
            ]
            []
        , span [ class "danger" ] [ text appKeyError ]
        ]
    , div []
        [ button
            [ type_ "button", class "button", class "button-primary", onClick (AppConfigMsg InputAppKey) ]
            [ text "See" ]
        ]
    ]

------------
-- UPDATE --
------------

updateAppConfig : AppConfigMsg -> AppConfigExtender r -> AppConfigExtender r
updateAppConfig msg model =
  case msg of
    NewTeamName newTeamName -> { model | teamName = newTeamName }

    NewSheetId newSheetId -> { model | sheetId = newSheetId }

    NewApiKey newApiKey -> { model | apiKey = newApiKey }

    NewAppKey newAppKey -> { model | appKey = newAppKey }

    CreateAppConfig -> model

    CopiedAppKeys copiedAppKey -> { model | appKey = copiedAppKey }

    InputAppKey ->
      -- TODO: No way to avoid that boilerplate? Seriously?
      let
        maybeResult : Maybe AppConfig
        maybeResult = decodeAppConfigFromAppKey model.appKey

      in
        case maybeResult of
          Just result ->
            { model
            | teamName = result.teamName
            , sheetId = result.sheetId
            , apiKey = result.apiKey
            , isAdmin = result.isAdmin
            , appKeyError = ""
            }
          Nothing -> { model | appKeyError = "Invalid app key!" }
