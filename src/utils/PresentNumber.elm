module PresentNumber exposing (presentNumber)

type alias Seed =
  { result : String
  , index : Int
  }

accumulator : Char -> Seed -> Seed
accumulator digit seed = if modBy 3 seed.index == 0 && seed.index /= 0
  then { result = String.join "" [ String.fromChar digit, " ", seed.result ], index = seed.index + 1 }
  else { result = String.fromChar digit ++ seed.result, index = seed.index + 1 }


presentNumber : Int -> String
presentNumber number = String.fromInt number
  |> String.foldr accumulator ( Seed "" 0 )
  |> .result
