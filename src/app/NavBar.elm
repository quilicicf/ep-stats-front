module NavBar exposing (viewNavBar)

import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (withDefault)

import Msg exposing (..)
import Pagination exposing (Page(..), findPath, findName)

pagesLinked : List Page
pagesLinked = [ AlliancePage, TitansPage, WarsPage ]

viewNavBar : Page -> Html Msg
viewNavBar currentPage =
  let
    isCurrentPageLinkable : Bool
    isCurrentPageLinkable = List.member currentPage pagesLinked
  in
    nav
      [ class "nav-bar", hidden ( not isCurrentPageLinkable ) ]
      ( List.map ( viewNavBarElement currentPage ) pagesLinked )

viewNavBarElement : Page -> Page -> Html Msg
viewNavBarElement currentPage pageForLink =
  let
    cssClass : String
    cssClass =if currentPage == pageForLink
      then "nav-element active"
      else "nav-element inactive"
  in
    a
      [ href ( findPath pageForLink |> withDefault "#" ), class cssClass ]
      [ text ( findName pageForLink |> withDefault "Unknown page" ) ]
