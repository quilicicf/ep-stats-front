module Constants exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

empiresAndPuzzlesLink : Html msg
empiresAndPuzzlesLink = a [ href "https://forum.smallgiantgames.com/" ] [ text "Empires & Puzzles" ]

epStatsFrontRepositoryUrl : String
epStatsFrontRepositoryUrl = "https://github.com/quilicicf/ep-stats-front"
