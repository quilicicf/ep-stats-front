module Msg exposing (..)

import Browser exposing (UrlRequest)
import Http
import Url exposing (Url)

import Titans exposing (DetailedColor)

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
  | NewTitanPeriodSelected Int
  | NewTitanColorSelected DetailedColor
  | NewTitanStarsSelected  ( Maybe Int )
  | NewWarPeriodSelected Int
  | NewWarBonusSelected  ( Maybe String )
