port module Main exposing (main)

import Browser exposing (application, UrlRequest, Document)
import Browser.Navigation exposing (Key, load, pushUrl)

import Debug exposing (log)

import Html exposing (..)
import Html.Attributes exposing (..)

import Url exposing (Url)

import Json.Decode exposing (Value, Decoder)

import Msg exposing (..)
import AppConfig exposing (..)
import Pagination exposing (..)

port setStorage : StorageAppState -> Cmd msg

type alias Model =
  { appConfig: AppConfig
  , appKey: String -- AppState
  , currentPage: Page
  , navigationKey: Key
  }

init : Value -> Url -> Key -> (Model, Cmd Msg)
init flags url key =
  let
      initPage : Page
      initPage = findPage url

      appConfig : AppConfig
      appConfig = log "Initialized with" (decodeAppConfigFromJson flags)

      initModel: Model
      initModel = Model appConfig "" initPage key
  in
    ( initModel , Cmd.none )

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

    AppConfigMsg appConfigMsg ->
      case appConfigMsg of
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

view : Model -> Document Msg
view model =
  case model.currentPage of
    WelcomePage -> createDocument (welcomeScreen model)

    AppConfigPage -> createDocument (appConfigForm model.appConfig)

    AppKeyPage -> createDocument appKeyForm

    AppKeyCopierPage -> createDocument (appKeyCopierView model.appConfig)

    StatsPage -> createDocument (text "Stats should go here")

    NotFoundPage -> createDocument (text "Not found")

createDocument : Html Msg -> Document Msg
createDocument body =
  { title = "EP stats"
  , body = [
      div
        []
        [ h1 [] [ text "EP stats" ]
        , body
        ]
    ]
  }

welcomeScreen : Model -> Html Msg
welcomeScreen _ =
  div []
    [ text "Choose your side"
    , br [] []
    , a [ href "/appConfig" ] [ text "I'm the alliance's GOD" ]
    , br [] []
    , a [ href "/appKey" ] [ text "I'm but a peon" ]
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

