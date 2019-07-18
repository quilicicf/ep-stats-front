module Pagination exposing (Page(..), findPage, findPath, pushPage)

import AssocList as Dict exposing (Dict)
import Browser.Navigation exposing (Key, pushUrl)
import Url exposing (..)

import Msg exposing (..)

notFoundPath : String
notFoundPath = "/notFound"

pages : List (Page, String)
pages = [
--  Landing page
  ( AppKeyPage, "/" ),

-- Not accessible (at least in theory XD)
  ( AuthorizedPage, "/authorized" ),

--  App configuration
  ( AppConfigPage, "/appConfig" ),
  ( AppKeyCopierPage, "/appKeyCopy" ),

--  Stats
  ( AlliancePage, "/alliance" ),
  ( TitansPage, "/titans" ),
  ( WarsPage, "/wars" ),

--  Errors
  ( NotFoundPage, notFoundPath )
  ]

pagesToPaths : Dict Page String
pagesToPaths = Dict.fromList pages

pathsToPages : Dict String Page
pathsToPages = List.map (\(page, path) -> (path, page)) pages |> Dict.fromList

type Page
--  Landing page
  = AppKeyPage

-- Not accessible (at least in theory XD)
  | AuthorizedPage

--  App configuration
  | AppConfigPage
  | AppKeyCopierPage

--  Stats
  | AlliancePage
  | TitansPage -- TODO replace with titans page and wars page
  | WarsPage -- TODO replace with titans page and wars page

--  Errors
  | NotFoundPage

findPage : Url -> Maybe Page
findPage url = Dict.get url.path pathsToPages

findPath : Page -> Maybe String
findPath page = Dict.get page pagesToPaths

pushPage : Key -> Page -> Cmd Msg
pushPage key page = pushUrl key ( findPath page |> Maybe.withDefault notFoundPath )
