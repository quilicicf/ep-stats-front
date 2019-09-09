module AppConfig exposing (
  AppConfig, AppConfigExtender, StorageAppState,
  updateAppConfig, viewAppKeyCopier, viewAppKeyInput, viewAppConfig,
  decodeStorageAppState, decodeAppConfigFromAppKey
  )

import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (..)

import MaybeExtra exposing (maybeify)
import Sneacret exposing (..)
import Msg exposing (..)
import Translations exposing (..)

------------
-- MODELS --
------------

type alias Model r = TranslationsExtender ( AppConfigExtender r )

type alias AppConfig =
  { teamName: String
  , sheetId: String
  , adminKey: Maybe String
  , sheetKey: String
  }

type alias AppConfigExtender r =
  { r
  | teamName: String
  , sheetId: String
  , adminKey: Maybe String
  , sheetKey: String
  , appKey: String
  , appKeyError: String
  }

type alias StorageAppState =
  { appKey: String
  , accessToken: Maybe String
  , selectedLanguage: String
  }

-----------
-- UTILS --
-----------

encodeMaybe : ( a -> Encode.Value ) -> Maybe a -> Encode.Value
encodeMaybe encoder maybe = case maybe of
  Just value -> encoder value
  Nothing -> Encode.null

jsonifyAppConfig : AppConfigExtender r -> Bool -> Encode.Value
jsonifyAppConfig appConfig isAdmin =
  let
    adminKeyEncoder : Maybe String -> Encode.Value
    adminKeyEncoder adminKey = if isAdmin
      then encodeMaybe Encode.string adminKey
      else encodeMaybe Encode.string Nothing
  in
  Encode.object
      [ ("teamName", Encode.string appConfig.teamName)
      , ("sheetId", Encode.string appConfig.sheetId)
      , ("adminKey", adminKeyEncoder appConfig.adminKey)
      , ("sheetKey", Encode.string appConfig.sheetKey)
      ]

encodeAppConfig : AppConfigExtender r -> Bool -> String
encodeAppConfig appConfig isAdmin = jsonifyAppConfig appConfig isAdmin
  |> Encode.encode 0
  |> Sneacret.sneak "START_KEY|>" "<|END_KEY"
  |> Maybe.withDefault "FAILED MISERABLY"

appConfigDecoder : Decoder AppConfig
appConfigDecoder =
  Decode.succeed AppConfig
    |> Pipeline.required "teamName" Decode.string
    |> Pipeline.required "sheetId" Decode.string
    |> Pipeline.required "adminKey" ( Decode.nullable Decode.string )
    |> Pipeline.required "sheetKey" Decode.string

storageAppStateDecoder : Decoder StorageAppState
storageAppStateDecoder =
  Decode.succeed StorageAppState
    |> Pipeline.optional "appKey" Decode.string ""
    |> Pipeline.optional "accessToken" (maybe Decode.string) Nothing
    |> Pipeline.optional "selectedLanguage" Decode.string "defaultLanguage"

decodeStorageAppState : Decode.Value -> StorageAppState
decodeStorageAppState appKeyAsJson =
  case Decode.decodeValue storageAppStateDecoder appKeyAsJson of
    Ok appState -> appState
    Err _ -> StorageAppState "" Nothing "defaultLanguage"

decodeAppConfig : String -> Maybe AppConfig
decodeAppConfig decodedAppKey =
  case Decode.decodeString appConfigDecoder decodedAppKey of
    Ok appConfig -> Just appConfig
    Err _ -> Nothing

decodeAppConfigFromAppKey : String -> Maybe AppConfig
decodeAppConfigFromAppKey encodedAppKey = Sneacret.unSneak encodedAppKey |> Maybe.andThen decodeAppConfig

----------
-- VIEW --
----------

viewAppConfig : Model r -> Html Msg
viewAppConfig model =
  Html.form [ class "configure-alliance" ]
    [ h2 [] [ text model.translations.configureYourAlliance ]
    , div [ class "form-field-inline" ]
      [ label [ for "teamName" ] [ text "Team name" ]
      , input [ type_ "text", id "teamName", Attributes.value model.teamName, onInput (AppConfigMsg << NewTeamName) ] []
      ]
    , div [ class "form-field-inline" ]
      [ label [ for "sheetId" ] [ text "Sheet id" ]
      , input [ type_ "text", id "sheetId", Attributes.value model.sheetId, onInput (AppConfigMsg << NewSheetId) ] []
      ]
    , div [ class "form-field-inline" ]
      [ label [ for "adminKey" ] [ text "Admin key" ]
      , input
        [ type_ "text"
        , id "adminKey"
        , Attributes.value <| Maybe.withDefault "" model.adminKey
        , onInput (AppConfigMsg << NewAdminKey << maybeify)
        ] []
      ]
    , div [ class "form-field-inline" ]
      [ label [ for "sheetKey" ] [ text "Sheet key" ]
      , input
        [ type_ "text"
        , id "sheetKey"
        , Attributes.value model.sheetKey
        , onInput (AppConfigMsg << NewSheetKey)
        ] []
      ]
    , div []
      [ button
        [ class "button", class "button-primary", type_ "button", onClick (AppConfigMsg CreateAppConfig) ]
        [ text model.translations.create ]
      ]
    ]

viewAppKeyCopier : Model r -> Html Msg
viewAppKeyCopier model =
  let
    adminAppKey : String
    adminAppKey = encodeAppConfig model True

    peonAppKey : String
    peonAppKey = encodeAppConfig model False

  in
    div [ class "app-key-form" ] [
      h2 [] [ text model.translations.copyTheKeysAndValidate ],
      div [ class "form-field" ] [
        label [] [ text model.translations.adminKey ],
        textarea [ id "admin-key", class "app-key-container", readonly True, rows 5, cols 80 ] [ text adminAppKey ],
        button
          [ type_ "button", class "button", class "button-secondary", attribute "data-copy-to-clipboard" "#admin-key" ]
          [ text model.translations.copy ]
      ],
      div [ class "form-field" ] [
        label [] [ text model.translations.peonKey ],
        textarea [ id "peon-key", class "app-key-container", readonly True, rows 5, cols 80 ] [ text peonAppKey ],
        button
          [ type_ "button", class "button", class "button-secondary", attribute "data-copy-to-clipboard" "#peon-key" ]
          [ text model.translations.copy ]
      ],
      button
        [ class "button", class "button-primary", type_ "button", onClick (AppConfigMsg (CopiedAppKeys adminAppKey)) ]
        [ text model.translations.iveStoredThemAway ]
    ]

viewAppKeyInput : Model r -> Html Msg
viewAppKeyInput { translations, appKeyError } =
  Html.form [ class "app-key-form" ]
    [ h2 [] [ text translations.pasteAppKey ]
    , div [ class "form-field-inline app-key-form-field" ]
        [ label [ for "appKey" ] [ text translations.appKey ]
        , textarea
            [ id "appKey"
            , class "app-key-container"
            , rows 5, cols 80
            , onInput (AppConfigMsg << NewAppKey)
            ]
            []
        , span [ class "danger" ] [ text appKeyError ]
        ]
    , div []
        [ button
            [ type_ "button", class "button", class "button-primary", onClick (AppConfigMsg InputAppKey) ]
            [ text translations.see ]
        ]
    ]

------------
-- UPDATE --
------------

updateAppConfig : AppConfigMsg -> Model r -> Model r
updateAppConfig msg model =
  case msg of
    NewTeamName newTeamName -> { model | teamName = newTeamName }

    NewSheetId newSheetId -> { model | sheetId = newSheetId }

    NewAppKey newAppKey -> { model | appKey = newAppKey }

    CreateAppConfig -> model

    CopiedAppKeys copiedAppKey -> { model | appKey = copiedAppKey }

    InputAppKey ->
      decodeAppConfigFromAppKey model.appKey
        |> Maybe.map (\result ->
            { model
            | teamName = result.teamName
            , sheetId = result.sheetId
            , adminKey = result.adminKey
            , appKeyError = ""
            }
          )
        |> Maybe.withDefault { model | appKeyError = model.translations.invalidAppKey }

    NewAdminKey newAdminKey -> { model | adminKey = newAdminKey }

    NewSheetKey newSheetKey -> { model | sheetKey = newSheetKey }
