port module Main exposing (main)

import Browser exposing (application, UrlRequest, Document)
import Browser.Navigation exposing (Key, load, pushUrl)

import Debug exposing (log)

import Html exposing (..)
import Html.Attributes exposing (..)

import Url exposing (Url)

import Json.Decode exposing (Value, Decoder)

import Msg exposing (..)
import Pagination exposing (..)

import Stats exposing (StatsExtender,
  fetchWarStats, fetchTitanStats, updateStats, viewStats)
import TitanStats exposing (TitanStats)
import AllianceName exposing (allianceName)
import AppConfig exposing (AppConfig, AppConfigExtender, StorageAppState,
  decodeStorageAppState, decodeAppConfigFromAppKey,
  updateAppConfig, viewAppConfig, viewAppKeyInput, viewAppKeyCopier
  )

port setStorage : StorageAppState -> Cmd msg

type alias Model =
  -- AppConfig
  { teamName: String
  , sheetId: String
  , apiKey: String
  , isAdmin: Bool
  , appKey: String
  , appKeyError: String

  -- Stats
  , filteredMember : String
  , filteredPeriod : Int
  , titanStats: Maybe TitanStats
  , warStats: Maybe String

  -- Navigation
  , currentPage: Page
  , navigationKey: Key
  }

createInitialModel : Maybe AppConfig -> String -> Page -> Key -> Model
createInitialModel maybeAppConfig appKey initialPage key =
  case maybeAppConfig of
    Just appConfig -> Model
      -- App config
      appConfig.teamName
      appConfig.sheetId
      appConfig.apiKey
      appConfig.isAdmin
      appKey
      ""
      -- Stats
      allianceName 30 Nothing Nothing
      -- Navigation
      initialPage key

    Nothing -> Model
      -- App config
      "" "" "" False "" ""
      -- Stats
      allianceName 30 Nothing Nothing
      -- Navigation
      initialPage key

init : Value -> Url -> Key -> (Model, Cmd Msg)
init flags url key =
  let
      storageAppState : StorageAppState
      storageAppState = decodeStorageAppState flags

      maybeAppConfig : Maybe AppConfig
      maybeAppConfig = log "Initialized with" (decodeAppConfigFromAppKey storageAppState.appKey)

      hasAppConfig : Bool
      hasAppConfig = if maybeAppConfig == Nothing then False else True

      initialPage : Page
      initialPage = log "tata" (findInitialPage url hasAppConfig)

      initModel: Model
      initModel = createInitialModel maybeAppConfig storageAppState.appKey initialPage key

  in
    case initialPage of
      StatsPage ->
        ( initModel
        , Cmd.batch [
          fetchTitanStats initModel.sheetId initModel.apiKey,
          fetchWarStats initModel.sheetId initModel.apiKey
        ])
      _ -> ( initModel, Cmd.none )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
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

    AppConfigMsg appConfigMsg ->
      let
        newModel : Model
        newModel = updateAppConfig appConfigMsg model

      in

        case appConfigMsg of
          CreateAppConfig -> ( newModel, pushUrl model.navigationKey "/appKeyCopy")

          CopiedAppKeys appKey -> (
            newModel,
            Cmd.batch [
              setStorage (StorageAppState appKey),
              pushUrl model.navigationKey "/stats",
              fetchTitanStats model.sheetId model.apiKey,
              fetchWarStats model.sheetId model.apiKey
            ])

          InputAppKey -> (
            newModel,
            Cmd.batch [
              setStorage (StorageAppState model.appKey),
              pushUrl model.navigationKey "/stats",
              fetchTitanStats model.sheetId model.apiKey,
              fetchWarStats model.sheetId model.apiKey
            ])

          _ -> ( newModel, Cmd.none )

    StatsMsg statsMsg ->
      let
        newModel : Model
        newModel = updateStats statsMsg model
      in
        ( newModel, Cmd.none )

view : Model -> Document Msg
view model =
  case model.currentPage of
    WelcomePage -> createDocument model (welcomeScreen model)

    AppConfigPage -> createDocument model (viewAppConfig model)

    AppKeyPage -> createDocument model (viewAppKeyInput model)

    AppKeyCopierPage -> createDocument model (viewAppKeyCopier model)

    StatsPage -> createDocument model (viewStats model)

    NotFoundPage -> createDocument model (text "Not found")

createDocument : { r | teamName: String } -> Html Msg -> Document Msg
createDocument { teamName } body =
  let
    title: String
    title = if teamName == "" then "EP stats" else "EP stats - " ++ teamName
  in
    { title = title
    , body = [
        div
          []
          [ h1 [ class "header" ] [ text title ]
          , body
          ]
      ]
    }

welcomeScreen : Model -> Html Msg
welcomeScreen _ =
  div [ class "choose-your-side" ]
    [ h2 [] [ text "Choose your side" ]
    , div [ class "choices" ]
      [ a [ class "choice", class "choice-left", href "/appConfig" ] [ text "I'm the alliance's GOD" ]
      , a [ class "choice", class "choice-right", class "choice-main", href "/appKey" ] [ text "I'm but a peon" ]
      ]
    ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main : Program Value Model Msg
main = application
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlChange = UrlChanged
  , onUrlRequest = LinkClicked
  }

