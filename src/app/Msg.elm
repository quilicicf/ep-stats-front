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
  | NewAppKey String
  | CreateAppConfig
  | CopiedAppKey String
  | InputAppKey

type StatsMsg
  = GotStats (Result Error String)
  | GotRights (Result Error String)
  | BackToAppKeyMsg

type StatsFilterMsg
  = NewMemberSelected String
  | NewTitanPeriodSelected Int
  | NewTitanColorSelected TitanColor
  | NewTitanStarsSelected  ( Maybe Int )
  | NewWarPeriodSelected Int
  | NewWarBonusSelected  ( WarBonus )

type WelcomeMsg
  = ICreateKey
  | IHasKey
