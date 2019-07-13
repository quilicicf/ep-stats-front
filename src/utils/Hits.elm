module Hits exposing (Hits, computeHitsAverage)

type alias Hits = { totalDamage : Int, hitsNumber : Int }

computeHitsAverage : Hits -> Float
computeHitsAverage hits = ( toFloat hits.totalDamage ) / ( toFloat hits.hitsNumber )
