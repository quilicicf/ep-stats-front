port module Main exposing (main)

import Browser exposing (application, UrlRequest, Document)
import Browser.Navigation exposing (Key, load, pushUrl)

import Debug exposing (log)

import Html exposing (..)
import Html.Attributes exposing (..)

import Http exposing (..)

import Json.Decode exposing (Value, Decoder)

import Maybe exposing (withDefault)

import Url exposing (Url)

import Msg exposing (..)
import Pagination exposing (..)
import MaybeExtra exposing (hasValue)
import Spinner exposing (viewSpinner)
import Titans exposing (DetailedColor)
import Authorization exposing (makeAuthorizationUrl, readAccessToken)
import StatsFilter exposing (StatsFilterExtender, defaultStatsFilter, updateStatsFilters)
import Stats exposing (
  Stats, AllianceStats, MemberStats, FilteredStats, StatsExtender,
  fetchAllStats, updateStats, viewAllianceStats, viewTitansStats, viewWarsStats
  )
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

  -- Filters
  , filteredMember : String
  , filteredTitanPeriod : Int
  , filteredTitanColor : Maybe DetailedColor
  , filteredTitanStars : Maybe Int
  , filteredWarPeriod : Int
  , filteredWarBonus : Maybe String

  -- Stats
  , statsError : Maybe String
  , stats: Maybe Stats
  , allianceStats: Maybe AllianceStats
  , filteredStats: Maybe FilteredStats

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
    { teamName = appConfig.teamName
    , sheetId = appConfig.sheetId
    , isAdmin = appConfig.isAdmin
    , appKey = appKey
    , appKeyError = ""

    -- Authorization
    , accessToken = maybeAccessToken

    -- Stats
    , filteredMember = defaultStatsFilter.filteredMember
    , filteredTitanPeriod = defaultStatsFilter.filteredTitanPeriod
    , filteredTitanColor = defaultStatsFilter.filteredTitanColor
    , filteredTitanStars = defaultStatsFilter.filteredTitanStars
    , filteredWarPeriod = defaultStatsFilter.filteredWarPeriod
    , filteredWarBonus = defaultStatsFilter.filteredWarBonus
    , statsError = Nothing
    , stats = Nothing
    , allianceStats  = Nothing
    , filteredStats = Nothing

    -- Navigation
    , baseUrl = baseUrl
    , currentPage = AppKeyPage
    , navigationKey = key
    }

type InitCase = FirstVisit | WithAppKey | Authenticating | Authenticated

init : Value -> Url -> Key -> (Model, Cmd Msg)
init flags url key =
  let
      storageAppState : StorageAppState
      storageAppState = decodeStorageAppState flags

      maybeAppConfig : Maybe AppConfig
      maybeAppConfig = decodeAppConfigFromAppKey storageAppState.appKey

      loadingPage : Maybe Page
      loadingPage = findPage url

      initialCase : InitCase
      initialCase = if ( loadingPage == Just AuthorizedPage ) then Authenticating
        else if (hasValue maybeAppConfig) && (hasValue storageAppState.accessToken) then  Authenticated
        else if (hasValue maybeAppConfig) then WithAppKey
        else FirstVisit

      maybeAccessToken : Maybe String
      maybeAccessToken = readAccessToken url

      initModel: Model
      initModel = createInitialModel maybeAppConfig storageAppState.appKey storageAppState.accessToken key url

      initialPage : Page
      initialPage = withDefault AppKeyPage loadingPage

  in
    case initialCase of
      Authenticated -> (
        { initModel | currentPage = initialPage },
        Cmd.batch [
          fetchAllStats initModel.sheetId (withDefault "" initModel.accessToken),
          pushPage initModel.navigationKey initialPage
        ]
        )

      Authenticating -> (
        { initModel | currentPage = initialPage , accessToken = maybeAccessToken },
        Cmd.batch [
          setStorage ( StorageAppState initModel.appKey maybeAccessToken ),
          fetchAllStats initModel.sheetId (withDefault "" maybeAccessToken),
          pushPage initModel.navigationKey initialPage
        ]
        )

      WithAppKey -> ( initModel, load ( makeAuthorizationUrl initModel.baseUrl ) )

      FirstVisit -> ( initModel, pushPage initModel.navigationKey AppKeyPage )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url -> ( model, pushUrl model.navigationKey (Url.toString url) )

        Browser.External href -> ( model, load href )

    UrlChanged url ->
      let
        newPage : Page
        newPage = findPage url |> withDefault NotFoundPage
      in
        ( { model | currentPage = newPage }, Cmd.none )

    AppConfigMsg appConfigMsg ->
      let
        newModel : Model
        newModel = updateAppConfig appConfigMsg model

      in

        case appConfigMsg of
          CreateAppConfig -> ( newModel, pushPage model.navigationKey AppKeyCopierPage )

          CopiedAppKeys appKey -> (
            newModel,
            Cmd.batch [
              setStorage (StorageAppState appKey model.accessToken),
              pushPage model.navigationKey AlliancePage,
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

    StatsFilterMsg statsFilterMsg -> ( updateStatsFilters statsFilterMsg model, Cmd.none )

view : Model -> Document Msg
view model =
  case model.currentPage of
    AppConfigPage -> createDocument model (viewAppConfig model)

    AppKeyPage -> createDocument model (viewAppKeyInput model)

    AppKeyCopierPage -> createDocument model (viewAppKeyCopier model)

    AlliancePage -> createDocument model (viewAllianceStats model)

    TitansPage -> createDocument model (viewTitansStats model)

    WarsPage -> createDocument model (viewWarsStats model)

    NotFoundPage -> createDocument model (text "Not found")

    AuthorizedPage -> createDocument model ( viewSpinner "Authenticating..." )


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

