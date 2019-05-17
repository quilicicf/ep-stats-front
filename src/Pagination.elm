module Pagination exposing (..)

import Url exposing (..)

type Page
  = WelcomePage
  | AppConfigPage
  | AppKeyPage
  | AppKeyCopierPage
  | StatsPage
  | NotFoundPage

findPage : Url -> Page
findPage url =
  case url.path of
    "/" -> WelcomePage
    "/appConfig" -> AppConfigPage
    "/appKey" -> AppKeyPage
    "/stats" -> StatsPage
    _ -> NotFoundPage
