module Pagination exposing (..)

import Url exposing (..)

type Page
--  Landing page
  = AppKeyPage

--  App configuration
  | AppConfigPage
  | AppKeyCopierPage

--  Stats
  | StatsPage

--  Errors
  | NotFoundPage

findPage : Url -> Page
findPage url =
  case url.path of
--  Landing page
    "/" -> AppKeyPage

--  App configuration
    "/appConfig" -> AppConfigPage
    "/appKeyCopy" -> AppKeyCopierPage

--  Stats
    "/stats" -> StatsPage

--  Errors
    _ -> NotFoundPage
