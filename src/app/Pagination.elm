module Pagination exposing (..)

import Url exposing (..)

type Page
--  Landing page
  = AppKeyPage

--  App configuration
  | AppConfigPage
  | AppKeyCopierPage

--  Stats
  | AlliancePage
  | StatsPage -- TODO replace with titans page and wars page

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
    "/alliance" -> AlliancePage
    "/stats" -> StatsPage

--  Errors
    _ -> NotFoundPage
