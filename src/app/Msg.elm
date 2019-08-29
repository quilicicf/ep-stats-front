module Msg exposing (..)

import Browser exposing (UrlRequest)
import Http
import Url exposing (Url)

import Wars exposing (WarBonus)
import Titans exposing (DetailedColor)
import Internationalization exposing (Language)

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url
  | WelcomeMsg WelcomeMsg
  | LanguageUpdated Language
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
  | NewWarBonusSelected  ( WarBonus )

type WelcomeMsg
  = ICreateKey
  | IHasKey
