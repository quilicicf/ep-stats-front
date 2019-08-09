module Pagination exposing (Page(..), findPage, findPath, findName, pushPage)

import AssocList as Dict exposing (Dict)
import Browser.Navigation exposing (Key, pushUrl)
import Tuple3 exposing (..)
import Url exposing (..)

import Msg exposing (..)

notFoundPath : String
notFoundPath = "/notFound"

pages : List (Page, String, String)
pages = [
--  Landing page
  ( AppKeyPage, "/", "App key" ),

-- Not accessible (at least in theory XD)
  ( AuthorizedPage, "/authorized", "Authorization callback" ),

--  App configuration
  ( AppConfigPage, "/appConfig", "App config" ),
  ( AppKeyCopierPage, "/appKeyCopy", "App key copy" ),

--  Stats
  ( AlliancePage, "/alliance", "Alliance" ),
  ( TitansPage, "/titans", "Titans" ),
  ( WarsPage, "/wars", "Wars" ),

--  Errors
  ( NotFoundPage, notFoundPath, "Not found" )
  ]

pagesToPaths : Dict Page String
pagesToPaths = List.map (\page -> (Tuple3.first page, Tuple3.second page) ) pages |> Dict.fromList

pathsToPages : Dict String Page
pathsToPages = List.map (\page -> (Tuple3.second page, Tuple3.first page) ) pages |> Dict.fromList

pagesToNames : Dict Page String
pagesToNames = List.map (\page -> (Tuple3.first page, Tuple3.third page) ) pages |> Dict.fromList

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
  | TitansPage
  | WarsPage

--  Errors
  | NotFoundPage

findPage : Url -> Maybe Page
findPage url = Dict.get url.path pathsToPages

findPath : Page -> Maybe String
findPath page = Dict.get page pagesToPaths

findName : Page -> Maybe String
findName page = Dict.get page pagesToNames

pushPage : Key -> Page -> Cmd Msg
pushPage key page = pushUrl key ( findPath page |> Maybe.withDefault notFoundPath )
