module Pagination exposing (Page(..), findPage, findPath, findName, pushPage)

import AssocList as Dict exposing (Dict)
import Browser.Navigation exposing (Key, pushUrl)
import Translations exposing (Translations)
import Tuple3 exposing (..)
import Url exposing (..)

import Msg exposing (..)

notFoundPath : String
notFoundPath = "/notFound"

pages : List (Page, String, (Translations -> String))
pages = [
--  Landing page
  ( WelcomePage, "/", .welcome ),

--  Not accessible (at least in theory XD)
  ( AuthorizedPage, "/authorized", .authorizationCallback ),

--  App configuration
  ( AppKeyPage, "/appKey", .appKey ),
  ( AppConfigPage, "/appConfig",  .appConfig),
  ( AppKeyCopierPage, "/appKeyCopy", .appKeyCopy ),

--  Stats
  ( AlliancePage, "/alliance", .alliance ),
  ( TitansPage, "/titans", .titans ),
  ( WarsPage, "/wars", .wars ),

--  Static content
  ( PrivacyPolicy, "/privacy-policy", .privacyPolicy ),

--  Errors
  ( NotFoundPage, notFoundPath, .notFound )
  ]

pagesToPaths : Dict Page String
pagesToPaths = List.map (\page -> (Tuple3.first page, Tuple3.second page) ) pages |> Dict.fromList

pathsToPages : Dict String Page
pathsToPages = List.map (\page -> (Tuple3.second page, Tuple3.first page) ) pages |> Dict.fromList

pagesToNameGetters : Dict Page (Translations -> String)
pagesToNameGetters = List.map (\page -> (Tuple3.first page, Tuple3.third page) ) pages |> Dict.fromList

type Page
--  Landing page
  = WelcomePage

-- Not accessible (at least in theory XD)
  | AuthorizedPage

--  App configuration
  | AppKeyPage
  | AppConfigPage
  | AppKeyCopierPage

--  Stats
  | PrivacyPolicy
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

findName : Page -> Translations -> Maybe String
findName page translations = Dict.get page pagesToNameGetters |> Maybe.map (\getter -> getter translations)

pushPage : Key -> Page -> Cmd Msg
pushPage key page = pushUrl key ( findPath page |> Maybe.withDefault notFoundPath )
