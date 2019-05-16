port module Main exposing (main)

import Browser exposing (application, UrlRequest, Document)
import Browser.Navigation exposing (Key, load, pushUrl)

import Debug exposing (log)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Url exposing (Url)

import Base64 exposing (..)

import Result exposing (Result)

import Json.Decode as Decode exposing (Value, Decoder, string, bool)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (required, optional)

port setStorage : StorageAppState -> Cmd msg

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url
  | NewTeamName String
  | NewSheetId String
  | NewApiKey String
  | NewAppKey String
  | CreateAppConfig
  | CopiedAppKeys String
  | InputAppKey

type alias StorageAppState = { appKey: String }

type alias AppConfig =
  { teamName: String
  , sheetId: String
  , apiKey: String
  , isAdmin: Bool
  }

--type AppState = String

type Page
  = WelcomePage
  | AppConfigPage
  | AppKeyPage
  | AppKeyCopierPage
  | StatsPage
  | NotFoundPage

type alias Model =
  { appConfig: AppConfig
  , appKey: String -- AppState
  , currentPage: Page
  , navigationKey: Key
  }

jsonifyAppConfig : AppConfig -> Bool -> Value
jsonifyAppConfig appConfig isAdmin =
  Encode.object
      [ ("teamName", Encode.string appConfig.teamName)
      , ("sheetId", Encode.string appConfig.sheetId)
      , ("apiKey", Encode.string appConfig.apiKey)
      , ("isAdmin", Encode.bool isAdmin)
      ]

encodeAppConfig : AppConfig -> Bool -> String
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
        Err _ -> AppConfig "" "" "" False
    Err _ -> AppConfig "" "" "" False

welcomeScreen : Model -> Html Msg
welcomeScreen _ =
  div []
    [ text "Choose your side"
    , br [] []
    , a [ href "/appConfig" ] [ text "I'm the alliance's GOD" ]
    , br [] []
    , a [ href "/appKey" ] [ text "I'm but a peon" ]
    ]

appConfigForm : AppConfig -> Html Msg
appConfigForm appConfig =
  Html.form []
    [ div []
        [ text "Team name"
        , br [] []
        , input
            [ type_ "text"
            , value appConfig.teamName
            , onInput NewTeamName
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
            , onInput NewSheetId
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
            , onInput NewApiKey
            ]
            []
        ]
    , br [] []
    , div []
        [ button
            [ type_ "button", onClick CreateAppConfig ]
            [ text "Create" ]
        ]
    ]

appKeyCopierView : AppConfig -> Html Msg
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
        [ type_ "button", onClick (CopiedAppKeys adminAppKey) ]
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
              , onInput NewAppKey
              ]
              []
          ]
      , br [] []
      , div []
          [ button
              [ type_ "button", onClick InputAppKey ]
              [ text "See" ]
          ]
      ]

init : Value -> Url -> Key -> (Model, Cmd Msg)
init flags url key =
  let
      initPage : Page
      initPage = findPage url

      appConfig : AppConfig
      appConfig = decodeAppConfigFromJson flags

      ignored = log "Initialized with" appConfig

      initModel: Model
      initModel = Model appConfig "" initPage key
  in
    ( initModel , Cmd.none )

findPage : Url -> Page
findPage url =
  case url.path of
    "/" -> WelcomePage
    "/appConfig" -> AppConfigPage
    "/appKey" -> AppKeyPage
    "/stats" -> StatsPage
    _ -> NotFoundPage

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ appConfig } as model) =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url -> ( model, pushUrl model.navigationKey (Url.toString url) )

        Browser.External href -> ( model, load href )

    UrlChanged url ->
      let
        newPage: Page
        newPage= findPage url
      in
        log url.path
        ( { model | currentPage = newPage }, Cmd.none )

    NewTeamName newTeamName ->
      let
        newAppConfig : AppConfig
        newAppConfig = { appConfig | teamName = newTeamName }
      in
        ( { model | appConfig = newAppConfig }, Cmd.none )

    NewSheetId newSheetId ->
      let
        newAppConfig : AppConfig
        newAppConfig = { appConfig | sheetId = newSheetId }
      in
        ( { model | appConfig = newAppConfig }, Cmd.none )

    NewApiKey newApiKey ->
      let
        newAppConfig : AppConfig
        newAppConfig = { appConfig | apiKey = newApiKey }
      in
        ( { model | appConfig = newAppConfig }, Cmd.none )

    NewAppKey newAppKey ->
      ( { model | appKey = log "New app key" newAppKey }, Cmd.none )

    CreateAppConfig -> ( { model | currentPage = AppKeyCopierPage }, Cmd.none)

    CopiedAppKeys appKey -> (
      model,
      Cmd.batch [
        setStorage (StorageAppState appKey),
        pushUrl model.navigationKey "/stats"
      ])

    InputAppKey -> (
      { model | appConfig = decodeAppConfigFromAppKey model.appKey },
      Cmd.batch [
        setStorage (StorageAppState model.appKey),
        pushUrl model.navigationKey "/stats"
      ])


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Document Msg
view model =
  case model.currentPage of
    WelcomePage -> (
      { title = "EP stats"
      , body = [
          div
            []
            [ h1 [] [ text "EP stats" ]
            , welcomeScreen model
            ]
        ]
      })

    AppConfigPage -> (
      { title = "EP stats"
      , body = [
          div []
            [ h1 [] [ text "EP stats" ]
            , appConfigForm model.appConfig
            ]
        ]
      })

    AppKeyPage -> (
      { title = "EP stats"
      , body = [
          div []
            [ h1 [] [ text "EP stats" ]
            , appKeyForm
            ]
        ]
      })

    AppKeyCopierPage -> (
      { title = "EP stats"
      , body = [
          div []
            [ h1 [] [ text "EP stats" ]
            , appKeyCopierView model.appConfig
            ]
        ]
      })

    StatsPage -> (
      { title = "EP stats"
      , body = [
          div []
            [ h1 [] [ text "Stats should go here" ] ]
        ]
      })

    NotFoundPage -> (
      { title = "EP stats"
      , body = [
          div []
            [ h1 [] [ text "Not found" ] ]
        ]
      })



main : Program Value Model Msg
main = application
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlChange = UrlChanged
  , onUrlRequest = LinkClicked
  }

