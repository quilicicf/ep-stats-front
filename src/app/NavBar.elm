module NavBar exposing (viewNavBar)

import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (..)

import MaybeExtra exposing (..)
import Msg exposing (..)
import Translations exposing (..)
import Pagination exposing (..)

pagesLinked : List Page
pagesLinked = [ AlliancePage, TitansPage, WarsPage ]

viewNavBar : Page -> Maybe String ->  Translations -> Html Msg
viewNavBar currentPage statsError translations =
  let
    isCurrentPageLinkable : Bool
    isCurrentPageLinkable = List.member currentPage pagesLinked && ( not <| hasValue statsError )
  in
    nav
      [ class "nav-bar", hidden ( not isCurrentPageLinkable ) ]
      ( List.map ( viewNavBarElement currentPage translations ) pagesLinked )

viewNavBarElement : Page -> Translations -> Page -> Html Msg
viewNavBarElement currentPage translations pageForLink =
  let
    cssClass : String
    cssClass =if currentPage == pageForLink
      then "nav-element active"
      else "nav-element inactive"
  in
    a
      [ href ( findPath pageForLink |> withDefault "#" ), class cssClass ]
      [ text ( findName pageForLink translations |> withDefault "Unknown page" ) ]
