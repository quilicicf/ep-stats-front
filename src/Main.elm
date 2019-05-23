port module Main exposing (main)

import Browser exposing (application, UrlRequest, Document)
import Browser.Navigation exposing (Key, load, pushUrl)

import Debug exposing (log)

import Html exposing (..)
import Html.Attributes exposing (..)

import Url exposing (Url)

import Json.Decode exposing (Value, Decoder)

import Msg exposing (..)
import Stats exposing (..)
import AppConfig exposing (..)
import Pagination exposing (..)

port setStorage : StorageAppState -> Cmd msg

type alias Model =
  -- AppConfig
  { teamName: String
  , sheetId: String
  , apiKey: String
  , isAdmin: Bool
  , appKey: String

  -- Stats
  , stats: Maybe Stats

  -- Navigation
  , currentPage: Page
  , navigationKey: Key
  }

init : Value -> Url -> Key -> (Model, Cmd Msg)
init flags url key =
  let
      initialPage : Page
      initialPage = findPage url

      appConfig : AppConfig
      appConfig = log "Initialized with" (decodeAppConfigFromJson flags)

      initModel: Model
      initModel = Model "" "" "" False "" Nothing initialPage key
  in
    ( initModel , Cmd.none )

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
              fetchTitanStats model.sheetId model.apiKey,
              fetchWarStats model.sheetId model.apiKey,
              pushUrl model.navigationKey "/stats"
            ])

          InputAppKey -> (
            newModel,
            Cmd.batch [
              setStorage (StorageAppState model.appKey),
              pushUrl model.navigationKey "/stats"
            ])

          _ -> ( newModel, Cmd.none )

    StatsMsg statsMsg ->
      case statsMsg of
        GotTitanStats _ -> ( (log "Got titan stats" model), Cmd.none )
        GotWarStats _ -> ( (log "Got war stats" model), Cmd.none )

view : Model -> Document Msg
view model =
  case model.currentPage of
    WelcomePage -> createDocument (welcomeScreen model)

    AppConfigPage -> createDocument (appConfigForm model)

    AppKeyPage -> createDocument appKeyForm

    AppKeyCopierPage -> createDocument (appKeyCopierView model)

    StatsPage -> createDocument (waitingForStats model.stats)

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

