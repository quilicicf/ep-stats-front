module Msg exposing (..)

import Browser exposing (UrlRequest)

import Http

import Url exposing (Url)

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url
  | AppConfigMsg AppConfigMsg
  | StatsMsg StatsMsg
  | StatsFilterMsg StatsFilterMsg

type AppConfigMsg
  = NewTeamName String
  | NewSheetId String
  | NewAppKey String
  | CreateAppConfig
  | CopiedAppKeys String
  | InputAppKey

type StatsMsg
  = GotStats (Result Http.Error String)

type StatsFilterMsg
  = NewMemberSelected String
  | NewTitanPeriodSelected String
  | NewTitanColorSelected String
