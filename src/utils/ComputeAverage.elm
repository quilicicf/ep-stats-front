module ComputeAverage exposing (computeAverageDamage, computeAverageScore)

import MemberScore exposing (MemberScore, AverageMemberScore)

type alias HasDamage r = { r | damage : Int }

computeAverageDamage : List ( HasDamage a ) -> Float
computeAverageDamage list =
  let
    sum : Float
    sum = List.map .damage list
      |> List.foldl (\damage seed -> seed + damage) 0
      |> toFloat

  in
    sum / ( toFloat ( List.length list ) )

scoreAccumulator : MemberScore -> MemberScore -> MemberScore
scoreAccumulator score seed =
    { damage = seed.damage + score.damage, teamValue = seed.teamValue + score.teamValue }

computeAverageScore : List MemberScore -> AverageMemberScore
computeAverageScore list =
  let
    sum : MemberScore
    sum = List.foldl scoreAccumulator { damage = 0, teamValue = 0 } list

    size : Float
    size = toFloat ( List.length list )
  in
    { damage = ( toFloat sum.damage ) / size,
      teamValue = ( toFloat sum.teamValue ) / size
    }
