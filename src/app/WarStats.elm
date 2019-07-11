module WarStats exposing (WarStats, WarMemberStats, updateWarStats)

------------
-- MODELS --
------------

type alias WarStats =
  { dates : List String
  , warScores : List ( String ) -- Change to add war scores
  }

type alias WarMemberStats =
  { averageWarScore : Float
--  , preferredWarBonus : String
  , warTeamValue : Float
  }

------------
-- UPDATE --
------------

updateWarStats : String -> Maybe WarStats
updateWarStats _ = Just ( WarStats [] [] )
