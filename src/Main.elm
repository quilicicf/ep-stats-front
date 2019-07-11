port module Main exposing (main)

import Browser exposing (application, UrlRequest, Document)
import Browser.Navigation exposing (Key, load, pushUrl)

import Dict exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Http exposing (..)

import Json.Decode exposing (Value, Decoder)

import Maybe exposing (withDefault)

import Url exposing (Url)

import Msg exposing (..)
import Pagination exposing (..)

import MaybeExtra exposing (hasValue)

import Alliance exposing (viewAlliance)
import AllianceName exposing (allianceName)
import Authorization exposing (makeAuthorizationUrl, readAccessToken)
import Stats exposing (MemberStats, StatsExtender, fetchAllStats, updateStats, viewStats)
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
  , stats: Maybe ( Dict String MemberStats )
  , filteredStats: Maybe ( Dict String MemberStats )

  -- Navigation
  , baseUrl: Url
  , currentPage: Page
  , navigationKey: Key
  }

createInitialModel : Maybe AppConfig -> String -> Maybe String -> Key -> Url -> Model
createInitialModel maybeAppConfig appKey maybeAccessToken key landingUrl =
  let
    appConfig : AppConfig
    appConfig = withDefault (AppConfig "" "" False) maybeAppConfig

    baseUrl : Url
    baseUrl = { landingUrl | query = Nothing, fragment = Nothing, path = "" }

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
      baseUrl AppKeyPage key

type InitCase = FirstVisit | WithAppKey | Authenticating | Authenticated

init : Value -> Url -> Key -> (Model, Cmd Msg)
init flags url key =
  let
      storageAppState : StorageAppState
      storageAppState = decodeStorageAppState flags

      maybeAppConfig : Maybe AppConfig
      maybeAppConfig = decodeAppConfigFromAppKey storageAppState.appKey

      initialCase : InitCase
      initialCase = if (url.path == "/authorized") then Authenticating
        else if (hasValue maybeAppConfig) && (hasValue storageAppState.accessToken) then  Authenticated
        else if (hasValue maybeAppConfig) then WithAppKey
        else FirstVisit

      maybeAccessToken : Maybe String
      maybeAccessToken = readAccessToken url

      initModel: Model
      initModel = createInitialModel maybeAppConfig storageAppState.appKey storageAppState.accessToken key url

  in
    case initialCase of
      Authenticated -> (
        { initModel | currentPage = AlliancePage },
        Cmd.batch [
          fetchAllStats initModel.sheetId (withDefault "" initModel.accessToken),
          pushUrl initModel.navigationKey "/alliance"
        ]
        )

      Authenticating -> (
        { initModel | currentPage = AlliancePage , accessToken = maybeAccessToken },
        Cmd.batch [
          setStorage ( StorageAppState initModel.appKey maybeAccessToken ),
          fetchAllStats initModel.sheetId (withDefault "" maybeAccessToken),
          pushUrl initModel.navigationKey "/alliance"
        ]
        )

      WithAppKey -> ( initModel, load ( makeAuthorizationUrl initModel.baseUrl ) )

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
              fetchAllStats model.sheetId (withDefault "" model.accessToken)
            ])

          InputAppKey -> (
            newModel,
            Cmd.batch [
              setStorage (StorageAppState model.appKey model.accessToken),
              load ( makeAuthorizationUrl model.baseUrl )
            ])

          _ -> ( newModel, Cmd.none )

    StatsMsg statsMsg ->
      case statsMsg of
        GotStats httpResult ->
          case httpResult of
            Ok _ -> ( updateStats statsMsg model, Cmd.none )
            Err error ->
              case error of
                BadStatus status ->
                  case status of
                    401 -> ( model, load ( makeAuthorizationUrl model.baseUrl ) )
                    _ -> ( model, Cmd.none )

                _ -> ( model, Cmd.none )

        _ -> ( updateStats statsMsg model, Cmd.none )


view : Model -> Document Msg
view model =
  case model.currentPage of
    AppConfigPage -> createDocument model (viewAppConfig model)

    AppKeyPage -> createDocument model (viewAppKeyInput model)

    AppKeyCopierPage -> createDocument model (viewAppKeyCopier model)

    AlliancePage -> createDocument model (viewAlliance model)

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

