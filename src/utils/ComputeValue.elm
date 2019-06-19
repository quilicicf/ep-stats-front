module ComputeValue exposing (computeValue)

computeValue : Int -> Int -> Int -> Int
computeValue allianceScore membersNumber memberScore =
  let
      alliancePercentage : Float
      alliancePercentage = toFloat memberScore / toFloat allianceScore * 100

      expectedParticipation : Float
      expectedParticipation = 100.0 / toFloat membersNumber

      balance : Float
      balance = alliancePercentage - expectedParticipation

      memberValue : Float
      memberValue = balance * toFloat membersNumber
  in
    (round memberValue) + 100
