module Msg exposing (..)

import Browser exposing (UrlRequest)

import Http

import Url exposing (Url)

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url
  | AppConfigMsg AppConfigMsg
  | StatsMsg StatsMsg

type AppConfigMsg
  = NewTeamName String
  | NewSheetId String
  | NewAppKey String
  | CreateAppConfig
  | CopiedAppKeys String
  | InputAppKey

type StatsMsg
  = GotStats (Result Http.Error String)
  | NewMemberSelected String
  | NewPeriodSelected String

