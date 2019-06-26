module AppConfig exposing (
  AppConfig, AppConfigExtender, StorageAppState,
  updateAppConfig, viewAppKeyCopier, viewAppKeyInput, viewAppConfig,
  decodeStorageAppState, decodeAppConfigFromAppKey
  )

import Base64

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as Decode exposing(Value, Decoder, maybe, string, bool)
import Json.Encode as Encode

import Json.Decode.Pipeline exposing (required, optional)

import Msg exposing (..)

------------
-- MODELS --
------------

type alias AppConfig =
  { teamName: String
  , sheetId: String
  , isAdmin: Bool
  }

type alias AppConfigExtender r =
  { r
  | teamName: String
  , sheetId: String
  , isAdmin: Bool
  , appKey: String
  , appKeyError: String
  }

type alias StorageAppState =
  { appKey: String
  , accessToken: Maybe String
  }

-----------
-- UTILS --
-----------

jsonifyAppConfig : AppConfigExtender r -> Bool -> Value
jsonifyAppConfig appConfig isAdmin =
  Encode.object
      [ ("teamName", Encode.string appConfig.teamName)
      , ("sheetId", Encode.string appConfig.sheetId)
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
    |> required "isAdmin" bool

storageAppStateDecoder : Decoder StorageAppState
storageAppStateDecoder =
  Decode.succeed StorageAppState
    |> optional "appKey" string ""
    |> optional "accessToken" (maybe string) Nothing

decodeStorageAppState : Value -> StorageAppState
decodeStorageAppState appKeyAsJson =
  case Decode.decodeValue storageAppStateDecoder appKeyAsJson of
    Ok appState -> appState
    Err _ -> StorageAppState "" Nothing

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
  Html.form [ class "configure-alliance" ]
    [ h2 [] [ text "Configure your Alliance" ]
    , div [ class "form-field-inline" ]
      [ label [ for "teamName" ] [ text "Team name" ]
      , input [ type_ "text", id "teamName", value appConfig.teamName, onInput (AppConfigMsg << NewTeamName) ] []
      ]
    , div [ class "form-field-inline" ]
      [ label [ for "sheetId" ] [ text "Sheet id" ]
      , input [ type_ "text", id "sheetId", value appConfig.sheetId, onInput (AppConfigMsg << NewSheetId) ] []
      ]
    , div []
      [ button
        [ class "button", class "button-primary", type_ "button", onClick (AppConfigMsg CreateAppConfig) ]
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
    div [ class "copy-app-key" ] [
      h2 [] [ text "Copy the keys and validate" ],
      div [ class "form-field" ] [
        label [] [ text "Admin key" ],
        textarea [ id "admin-key", class "app-key-container", readonly True, rows 5, cols 80 ] [ text adminAppKey ],
        button
          [ type_ "button", class "button", class "button-secondary", attribute "data-copy-to-clipboard" "#admin-key" ]
          [ text "Copy!" ]
      ],
      div [ class "form-field" ] [
        label [] [ text "Peon key" ],
        textarea [ id "peon-key", class "app-key-container", readonly True, rows 5, cols 80 ] [ text peonAppKey ],
        button
          [ type_ "button", class "button", class "button-secondary", attribute "data-copy-to-clipboard" "#peon-key" ]
          [ text "Copy!" ]
      ],
      button
        [ class "button", class "button-primary", type_ "button", onClick (AppConfigMsg (CopiedAppKeys adminAppKey)) ]
        [ text "I've stored'em away" ]
    ]

viewAppKeyInput : AppConfigExtender r -> Html Msg
viewAppKeyInput { appKeyError } =
  Html.form [ class "input-app-key" ]
    [ h2 [] [ text "Paste your app key" ]
    , div [ class "form-field-inline" ]
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
            , isAdmin = result.isAdmin
            , appKeyError = ""
            }
          Nothing -> { model | appKeyError = "Invalid app key!" }
