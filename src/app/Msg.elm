module Msg exposing (..)

import Browser exposing (..)
import Http exposing (..)
import Url exposing (..)

import Wars exposing (..)
import Titans exposing (..)
import Internationalization exposing (..)

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
  | NewAdminKey ( Maybe String )
  | NewSheetKey String
  | NewAppKey String
  | CreateAppConfig
  | CopiedAppKeys String
  | InputAppKey

type StatsMsg
  = GotStats (Result Error String)
  | BackToAppKeyMsg

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
