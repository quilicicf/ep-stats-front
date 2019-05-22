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
  , appKey: String
  }

type alias AppConfigExtender r =
  { r
  | teamName: String
  , sheetId: String
  , apiKey: String
  , isAdmin: Bool
  , appKey: String
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
    |> required "appKey" string

storageAppStateDecoder : Decoder StorageAppState
storageAppStateDecoder =
  Decode.succeed StorageAppState
    |> optional "appKey" string ""

decodeStorageAppState : Value -> StorageAppState
decodeStorageAppState appKeyAsJson =
  case Decode.decodeValue storageAppStateDecoder appKeyAsJson of
    Ok appState -> appState
    Err _ -> StorageAppState ""

decodeAppConfigFromJson : Value -> AppConfig
decodeAppConfigFromJson appKeyAsJson =
  let
    storageAppState : StorageAppState
    storageAppState = decodeStorageAppState appKeyAsJson

  in
    decodeAppConfigFromAppKey storageAppState.appKey

decodeAppConfigFromAppKey : String -> AppConfig
decodeAppConfigFromAppKey appKeyInBase64 =
  case Base64.decode appKeyInBase64 of
    Ok decodedAppKey ->
      case Decode.decodeString appConfigDecoder decodedAppKey of
        Ok appConfig -> appConfig
        Err _ -> AppConfig "" "" "" False appKeyInBase64
    Err _ -> AppConfig "" "" "" False appKeyInBase64

----------
-- VIEW --
----------

appConfigForm : AppConfigExtender r -> Html Msg
appConfigForm appConfig =
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

appKeyCopierView : AppConfigExtender r -> Html Msg
appKeyCopierView appConfig =
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

appKeyForm : Html Msg
appKeyForm =
  let
    appKey : String
    appKey = ""

  in
    Html.form []
      [ div []
          [ text "App key"
          , br [] []
          , input
              [ type_ "text"
              , value appKey
              , onInput (AppConfigMsg << NewAppKey)
              ]
              []
          ]
      , br [] []
      , div []
          [ button
              [ type_ "button", onClick (AppConfigMsg InputAppKey) ]
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
        result : AppConfig
        result = decodeAppConfigFromAppKey model.appKey

      in
        { model
        | teamName = result.teamName
        , sheetId = result.sheetId
        , apiKey = result.apiKey
        , isAdmin = result.isAdmin
        }
