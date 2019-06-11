module GenericStatsFilter exposing (GenericStatsFilter, defaultGenericStatsFilter)

import AllianceName exposing (allianceName)

type alias GenericStatsFilter =
  { user : String
  , period : Int
  }

defaultGenericStatsFilter : GenericStatsFilter
defaultGenericStatsFilter = GenericStatsFilter allianceName 30
