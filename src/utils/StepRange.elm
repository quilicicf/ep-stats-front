module StepRange exposing (stepRange)

stepRange : Int -> Int -> Int -> List Int
stepRange lowerValue higherValue step =
  recursiveAdd higherValue lowerValue step []

recursiveAdd : Int -> Int -> Int -> List Int -> List Int
recursiveAdd currentValue lowerValue step currentList =
  if currentValue + step <= lowerValue
    then currentList
    else recursiveAdd ( currentValue - step ) lowerValue step ( currentValue :: currentList )
