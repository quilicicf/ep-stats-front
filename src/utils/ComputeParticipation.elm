module ComputeParticipation exposing (computeParticipation)

computeParticipation : List Int -> Float
computeParticipation damages =
  let
    eventsNumber : Int
    eventsNumber = List.length damages

    strikesNumber : Int
    strikesNumber = List.filter (\damage -> damage /= 0) damages |> List.length
  in
    (toFloat strikesNumber) / (toFloat eventsNumber)
