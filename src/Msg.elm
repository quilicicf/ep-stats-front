module Msg exposing (..)

import Browser exposing (UrlRequest)

import Url exposing (Url)


type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url
  | AppConfigMsg AppConfigMsg

type AppConfigMsg
  = NewTeamName String
  | NewSheetId String
  | NewApiKey String
  | NewAppKey String
  | CreateAppConfig
  | CopiedAppKeys String
  | InputAppKey
