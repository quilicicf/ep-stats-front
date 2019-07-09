module WarStats exposing (WarStats, updateWarStats)

------------
-- MODELS --
------------

type alias WarStats =
  { dates : List String
  , warScores : List ( String ) -- Change to add war scores
  }

------------
-- UPDATE --
------------

updateWarStats : String -> Maybe WarStats
updateWarStats _ = Just ( WarStats [] [] )
