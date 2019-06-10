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
    "/appKeyCopy" -> AppKeyCopierPage
    "/appKey" -> AppKeyPage
    "/stats" -> StatsPage
    _ -> NotFoundPage

findInitialPage : Url -> Bool -> Page
findInitialPage url hasAppConfig =
  let
    deductedPage : Page
    deductedPage = findPage url

  in
    if hasAppConfig then
      case deductedPage of
        StatsPage -> StatsPage
        _ -> WelcomePage
    else
      WelcomePage
