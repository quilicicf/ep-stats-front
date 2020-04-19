port module Main exposing (..)

import Browser exposing (..)
import Browser.Navigation exposing (..)

import CustomStyle exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Maybe exposing (..)
import PrivacyPolicy exposing (..)
import Url exposing (..)

import Svg
import Svg.Attributes

import Msg exposing (..)
import Gsheet exposing (..)
import Pagination exposing (..)
import Wars exposing (..)
import NavBar exposing (..)
import MaybeExtra exposing (..)
import Welcome exposing (..)
import Spinner exposing (..)
import Titans exposing (..)
import Translations exposing (..)
import Authorization exposing (..)
import StatsFilter exposing (..)
import Internationalization exposing (..)
import Stats exposing (..)
import AppConfig exposing (..)

port setStorage : StorageAppState -> Cmd msg

type alias Model =
  -- AppConfig
  { teamName: String
  , sheetId: String
  , appKey: String
  , appKeyError: String

  -- Authorization
  , accessToken: Maybe String

  -- Filters
  , filteredMember : String
  , filteredTitanPeriod : Int
  , filteredTitanColor : TitanColor
  , filteredTitanStars : Maybe Int
  , filteredWarPeriod : Int
  , filteredWarBonus : WarBonus

  -- Stats
  , isAdmin: Bool
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
    appConfig = withDefault (AppConfig "" "") maybeAppConfig

    baseUrl : Url
    baseUrl = { landingUrl | query = Nothing, fragment = Nothing, path = "" }

    defaultStatsFilter : StatsFilter
    defaultStatsFilter = createDefaultStatsFilter translations

  in
    { teamName = appConfig.teamName
    , sheetId = appConfig.sheetId
    , appKey = appKey
    , appKeyError = ""

    -- Authorization
    , accessToken = maybeAccessToken

    -- Stats
    , isAdmin = False
    , filteredMember = defaultStatsFilter.filteredMember
    , filteredTitanPeriod = defaultStatsFilter.filteredTitanPeriod
    , filteredTitanColor = defaultStatsFilter.filteredTitanColor
    , filteredTitanStars = defaultStatsFilter.filteredTitanStars
    , filteredWarPeriod = defaultStatsFilter.filteredWarPeriod
    , filteredWarBonus = defaultStatsFilter.filteredWarBonus

    -- Filters
    , stats = Nothing
    , statsError = Nothing
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

type InitCase = FirstVisit | WithAppKey | Authenticating | Authenticated | DirectAccess Page

guessInitialCase : Maybe Page -> Maybe AppConfig -> Maybe String -> InitCase
guessInitialCase loadingPageMaybe appConfigMaybe accessTokenMaybe =
  let
    hasAppConfig : Bool
    hasAppConfig = hasValue appConfigMaybe

    isReadyToSeeStats : Bool
    isReadyToSeeStats = hasAppConfig && hasValue accessTokenMaybe

    startStatsCase : InitCase
    startStatsCase = if hasAppConfig && isReadyToSeeStats then Authenticated
      else if hasAppConfig then WithAppKey
      else FirstVisit
  in
    case loadingPageMaybe of
      Just loadingPage ->
        case loadingPage of
          AuthorizedPage -> Authenticating
          AppKeyPage -> startStatsCase
          AlliancePage -> startStatsCase
          TitansPage ->startStatsCase
          WarsPage -> startStatsCase
          _ -> DirectAccess loadingPage
      Nothing -> startStatsCase

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
      initialCase = guessInitialCase loadingPage maybeAppConfig accessToken

      accessTokenFromUrl : Maybe String
      accessTokenFromUrl = readAccessToken url

      initialModel: Model
      initialModel = createInitialModel maybeAppConfig appKey accessToken key url language translations

  in
    case initialCase of
      Authenticated -> initAuthenticatedUser loadingPage initialModel

      Authenticating -> (
          { initialModel | currentPage = AuthorizedPage , accessToken = accessTokenFromUrl },
          Cmd.batch [
            setStorage ( StorageAppState initialModel.appKey accessTokenFromUrl languageAsString ),
            pushPage initialModel.navigationKey AlliancePage,
            fetchAllStats initialModel.sheetId (withDefault "" accessTokenFromUrl)
          ]
        )

      WithAppKey -> ( initialModel, load ( makeAuthorizationUrl initialModel.baseUrl ) )

      DirectAccess page -> ( { initialModel | currentPage = page }, Cmd.none )

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
            Ok statsAsString ->
              let
                fetchRightsCmd : Cmd Msg
                fetchRightsCmd = fetchAdminRights model.sheetId (withDefault "" model.accessToken)
              in
               updateStats statsAsString model fetchRightsCmd
            Err error ->
              case error of
                BadStatus status ->
                  case status of
                    401 -> ( model, load ( makeAuthorizationUrl model.baseUrl ) )
                    403 -> ( model, load ( makeAuthorizationUrl model.baseUrl ) )
                    _ -> ( model, Cmd.none )

                _ -> ( model, Cmd.none )

        GotRights httpResult -> case httpResult of
          Ok rightsAsString ->
            let
              isAdmin : Bool -- Hacky but efficient...
              isAdmin = String.contains "requestingUserCanEdit\": true" rightsAsString
            in
              ({ model | isAdmin = isAdmin }, Cmd.none)
          Err _ -> (model, Cmd.none) -- Can't get the info? Ya not admin

        BackToAppKeyMsg -> ( model, pushPage model.navigationKey AppKeyPage )

    StatsFilterMsg statsFilterMsg -> (
        updateStatsFilters statsFilterMsg model |> updateStatsWithFilter,
        Cmd.none
      )

    WelcomeMsg welcomeMsg ->
      case welcomeMsg of
        IHasKey -> ( { model | currentPage = AppKeyPage }, pushPage model.navigationKey AppKeyPage )
        ICreateKey -> ( { model | currentPage = AppConfigPage }, pushPage model.navigationKey AppConfigPage )

view : Model -> Document Msg
view model =
  case model.currentPage of
    AppConfigPage -> createDocumentWithDefaultTitle model (viewAppConfig model)

    AppKeyPage -> createDocumentWithDefaultTitle model (viewAppKeyInput model)

    AppKeyCopierPage -> createDocumentWithDefaultTitle model (viewAppKeyCopier model)

    AlliancePage -> createDocumentWithTeamName model (viewAllianceStats model)

    TitansPage -> createDocumentWithTeamName model (viewTitansStats model)

    WarsPage -> createDocumentWithTeamName model (viewWarsStats model)

    NotFoundPage -> createDocumentWithDefaultTitle model (text model.translations.notFound )

    AuthorizedPage -> createDocumentWithDefaultTitle model ( viewSpinner "" )

    PrivacyPolicy -> createDocument ( model.translations.appTitle ++ " - Privacy policy" ) model viewPrivacyPolicy

    WelcomePage -> createDocument ( model.translations.appTitle ++ " - " ++ model.translations.welcome ) model ( viewWelcome model.language)

createDocumentWithTeamName : Model -> Html Msg -> Document Msg
createDocumentWithTeamName model body =
  let
    title: String
    title = if model.teamName == ""
      then model.translations.appTitle
      else model.translations.appTitle ++ " - " ++ model.teamName
  in
    createDocument title model body

createDocumentWithDefaultTitle : Model -> Html Msg -> Document Msg
createDocumentWithDefaultTitle model body = createDocument model.translations.appTitle model body

createDocument : String -> Model -> Html Msg -> Document Msg
createDocument title { currentPage, language, statsError, translations } body =
  { title = title
  , body = [
      div
        []
        [ viewHeaderBar { title = title, language = language }
        , viewNavBar currentPage statsError translations
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
    [ Svg.svg [ Svg.Attributes.class "flag" ] [ Svg.use [ Svg.Attributes.xlinkHref ( "#flag-icon-css-" ++ languageAsString ) ] [] ] ]

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

