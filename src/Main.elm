port module Main exposing (main)

import Browser exposing (application, UrlRequest, Document)
import Browser.Navigation exposing (Key, load, pushUrl)

import CustomStyle exposing (customStyle)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (Value, Decoder)
import Maybe exposing (withDefault)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Url exposing (Url)

import Msg exposing (..)
import Pagination exposing (..)
import Wars exposing (WarBonus)
import NavBar exposing (viewNavBar)
import MaybeExtra exposing (hasValue)
import Spinner exposing (viewSpinner)
import Titans exposing (DetailedColor)
import Translations exposing (Translations)
import Authorization exposing (makeAuthorizationUrl, readAccessToken)
import StatsFilter exposing (StatsFilter, StatsFilterExtender, createDefaultStatsFilter, updateStatsFilters)
import Internationalization exposing (Language(..), languages, findLanguage, languageToString, getTranslations)
import Stats exposing (
  Stats, AllianceStats, MemberStats, FilteredStats, StatsExtender,
  fetchAllStats, updateStats, updateStatsWithFilter,
  viewAllianceStats, viewTitansStats, viewWarsStats
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
  , filteredTitanColor : DetailedColor
  , filteredTitanStars : Maybe Int
  , filteredWarPeriod : Int
  , filteredWarBonus : WarBonus

  -- Stats
  , stats: Maybe Stats
  , statsError : Maybe String
  , allianceStats: Maybe AllianceStats
  , filteredStats: Maybe FilteredStats

  -- Navigation
  , baseUrl: Url
  , currentPage: Page
  , navigationKey: Key

  -- Internationalization
  , language: Language
  , translations: Translations
  }

createInitialModel : Maybe AppConfig -> String -> Maybe String -> Key -> Url -> Language -> Translations -> Model
createInitialModel maybeAppConfig appKey maybeAccessToken key landingUrl language translations =
  let
    appConfig : AppConfig
    appConfig = withDefault (AppConfig "" "" False) maybeAppConfig

    baseUrl : Url
    baseUrl = { landingUrl | query = Nothing, fragment = Nothing, path = "" }

    defaultStatsFilter : StatsFilter
    defaultStatsFilter = createDefaultStatsFilter translations

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

    -- Internationalization
    , language = language
    , translations = translations
    }

type InitCase = FirstVisit | WithAppKey | Authenticating | Authenticated

init : Value -> Url -> Key -> (Model, Cmd Msg)
init flags url key =
  let
      { appKey, accessToken, selectedLanguage } = decodeStorageAppState flags

      language : Language
      language = findLanguage selectedLanguage

      languageAsString : String
      languageAsString = languageToString language

      translations : Translations
      translations = getTranslations language

      maybeAppConfig : Maybe AppConfig
      maybeAppConfig = decodeAppConfigFromAppKey appKey

      loadingPage : Maybe Page
      loadingPage = findPage url

      initialCase : InitCase
      initialCase = if ( loadingPage == Just AuthorizedPage ) then Authenticating
        else if (hasValue maybeAppConfig) && (hasValue accessToken) then  Authenticated
        else if (hasValue maybeAppConfig) then WithAppKey
        else FirstVisit

      maybeAccessToken : Maybe String
      maybeAccessToken = readAccessToken url

      initialModel: Model
      initialModel = createInitialModel maybeAppConfig appKey accessToken key url language translations

  in
    case initialCase of
      Authenticated -> initAuthenticatedUser loadingPage initialModel

      Authenticating -> (
          { initialModel | currentPage = AuthorizedPage , accessToken = maybeAccessToken },
          Cmd.batch [
            setStorage ( StorageAppState initialModel.appKey maybeAccessToken languageAsString ),
            pushPage initialModel.navigationKey AlliancePage,
            fetchAllStats initialModel.sheetId (withDefault "" maybeAccessToken)
          ]
        )

      WithAppKey -> ( initialModel, load ( makeAuthorizationUrl initialModel.baseUrl ) )

      FirstVisit -> ( initialModel, pushPage initialModel.navigationKey AppKeyPage )

initAuthenticatedUser : Maybe Page -> Model ->  (Model, Cmd Msg)
initAuthenticatedUser maybeLoadingPage initialModel =
  let
    pageToLoad : Page
    pageToLoad =
      case maybeLoadingPage of
        Nothing -> AlliancePage
        Just loadingPage ->
          case loadingPage of
            TitansPage -> TitansPage
            WarsPage -> WarsPage
            _ -> AlliancePage
  in
    (
      { initialModel | currentPage = pageToLoad },
      Cmd.batch [
        fetchAllStats initialModel.sheetId (withDefault "" initialModel.accessToken),
        pushPage initialModel.navigationKey pageToLoad
      ]
    )

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

    LanguageUpdated newLanguage ->
      ( { model | language = newLanguage, translations = getTranslations newLanguage }
      , setStorage ( StorageAppState model.appKey model.accessToken ( newLanguage |> languageToString ) )
      )

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
              setStorage ( StorageAppState appKey model.accessToken (model.language |> languageToString) ),
              pushPage model.navigationKey AlliancePage,
              fetchAllStats model.sheetId (withDefault "" model.accessToken)
            ])

          InputAppKey -> (
            newModel,
            Cmd.batch [
              setStorage ( StorageAppState model.appKey model.accessToken (model.language |> languageToString) ),
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

    StatsFilterMsg statsFilterMsg -> (
        updateStatsFilters statsFilterMsg model |> updateStatsWithFilter,
        Cmd.none
      )

view : Model -> Document Msg
view model =
  case model.currentPage of
    AppConfigPage -> createDocument model (viewAppConfig model)

    AppKeyPage -> createDocument model (viewAppKeyInput model)

    AppKeyCopierPage -> createDocument model (viewAppKeyCopier model)

    AlliancePage -> createDocument model (viewAllianceStats model)

    TitansPage -> createDocument model (viewTitansStats model)

    WarsPage -> createDocument model (viewWarsStats model)

    NotFoundPage -> createDocument model (text model.translations.notFound )

    AuthorizedPage -> createDocument model ( viewSpinner "" )


createDocument : { r | teamName: String, currentPage: Page, language: Language, translations: Translations } -> Html Msg -> Document Msg
createDocument { teamName, currentPage, language, translations } body =
  let
    title: String
    title = if teamName == ""
      then translations.appTitle
      else translations.appTitle ++ " - " ++ teamName
  in
    { title = title
    , body = [
        div
          []
          [ viewHeaderBar { title = title, language = language }
          , (viewNavBar currentPage translations)
          , body
          ]
      ]
    }

viewHeaderBar : { r | title: String, language: Language } -> Html Msg
viewHeaderBar { title, language } =
  let
    style : Attribute msg
    style = customStyle [ ("--languages-number", List.length languages |> String.fromInt) ]
  in
    div [ class "header-bar", style ] [
      h1 [ class "header" ] [ text title ],
      viewLanguageSwitcher language
    ]

viewLanguageSwitcher : Language -> Html Msg
viewLanguageSwitcher appLanguage =
  div
    [ class "language-switcher" ]
    ( List.map ( viewLanguage appLanguage ) languages )

viewLanguage : Language -> ( String, Language ) -> Html Msg
viewLanguage appLanguage (languageAsString, language) =
  button
    [ class "language", disabled ( appLanguage == language ), onClick ( LanguageUpdated language )  ]
    [ svg [ Svg.Attributes.class "flag" ] [ use [ xlinkHref ( "#flag-icon-css-" ++ languageAsString ) ] [] ] ]

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

