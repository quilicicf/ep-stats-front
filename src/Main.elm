port module Main exposing (main)

import Browser exposing (application, UrlRequest, Document)
import Browser.Navigation exposing (Key, load, pushUrl)

import Debug exposing (log)

import Html exposing (..)
import Html.Attributes exposing (..)

import Json.Decode exposing (Value, Decoder)

import Maybe exposing (withDefault)
import MaybeExtra exposing (withLoggedDefault)

import Url exposing (Url)

import Msg exposing (..)
import Pagination exposing (..)

import MaybeExtra exposing (hasValue)
import TitanStats exposing (TitanStats)
import AllianceName exposing (allianceName)
import Authorization exposing (authorizationUrl, readAccessToken)
import Stats exposing (StatsExtender, fetchAllStats, updateStats, viewStats)
import AppConfig exposing (AppConfig, AppConfigExtender, StorageAppState,
  decodeStorageAppState, decodeAppConfigFromAppKey,
  updateAppConfig, viewAppConfig, viewAppKeyInput, viewAppKeyCopier
  )

port setStorage : StorageAppState -> Cmd msg

type alias Model =
  -- AppConfig
  { teamName: String
  , sheetId: String
  , isAdmin: Bool
  , appKey: String
  , appKeyError: String

  -- Authorization
  , accessToken: Maybe String

  -- Stats
  , filteredMember : String
  , filteredPeriod : Int
  , titanStats: Maybe TitanStats
  , warStats: Maybe String

  -- Navigation
  , currentPage: Page
  , navigationKey: Key
  }

createInitialModel : Maybe AppConfig -> String -> Maybe String -> Key -> Model
createInitialModel maybeAppConfig appKey maybeAccessToken key =
  let
    appConfig : AppConfig
    appConfig = withDefault (AppConfig "" "" False) maybeAppConfig

  in
    Model
      -- App config
      appConfig.teamName
      appConfig.sheetId
      appConfig.isAdmin
      appKey
      ""
      -- Authorization
      maybeAccessToken
      -- Stats
      allianceName 30 Nothing Nothing
      -- Navigation
      AppKeyPage key

type InitCase = FirstVisit | WithAppKey | Authenticating | Authenticated

init : Value -> Url -> Key -> (Model, Cmd Msg)
init flags url key =
  let
      storageAppState : StorageAppState
      storageAppState = log "Initialized with" (decodeStorageAppState flags)

      maybeAppConfig : Maybe AppConfig
      maybeAppConfig = decodeAppConfigFromAppKey storageAppState.appKey

      initialCase : InitCase
      initialCase = if (url.path == "/authorized") then Authenticating
        else if (hasValue maybeAppConfig) && (hasValue storageAppState.accessToken) then  Authenticated
        else if (hasValue maybeAppConfig) then WithAppKey
        else FirstVisit

      maybeAccessToken : Maybe String
      maybeAccessToken = log "Access token from URL" (readAccessToken url)

      initModel: Model
      initModel = createInitialModel maybeAppConfig storageAppState.appKey storageAppState.accessToken key

  in
    case initialCase of
      Authenticated -> (
        { initModel | currentPage = StatsPage },
        Cmd.batch [
          fetchAllStats initModel.sheetId (withLoggedDefault "" initModel.accessToken),
          pushUrl initModel.navigationKey "/stats"
        ]
        )

      Authenticating -> (
        { initModel | currentPage = StatsPage , accessToken = maybeAccessToken },
        Cmd.batch [
          setStorage ( StorageAppState initModel.appKey maybeAccessToken ),
          fetchAllStats initModel.sheetId (withLoggedDefault "" maybeAccessToken),
          pushUrl initModel.navigationKey "/stats"
        ]
        )

      WithAppKey -> ( initModel, load authorizationUrl )

      FirstVisit -> ( initModel, pushUrl initModel.navigationKey "/" )


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
              setStorage (StorageAppState appKey model.accessToken),
              pushUrl model.navigationKey "/stats",
              fetchAllStats model.sheetId (withLoggedDefault "" model.accessToken)
            ])

          InputAppKey -> (
            newModel,
            Cmd.batch [
              setStorage (StorageAppState model.appKey model.accessToken),
              load authorizationUrl
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

