module ComputeAverage exposing (computeAverageDamage, computeAverageScore)

import MaybeExtra exposing (hasValue)
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

scoreAccumulator : Maybe MemberScore -> MemberScore -> MemberScore
scoreAccumulator maybeScore seed =
  case maybeScore of
    Just score -> { damage = seed.damage + score.damage, teamValue = seed.teamValue + score.teamValue }
    Nothing -> seed

computeAverageScore : List ( Maybe MemberScore ) -> AverageMemberScore
computeAverageScore list =
  let
    sum : MemberScore
    sum = List.foldl scoreAccumulator { damage = 0, teamValue = 0 } list

    isComplete : Bool
    isComplete = ( List.filter hasValue list |> List.length ) == List.length list

    size : Float
    size = toFloat ( List.length list )
  in
    { isComplete = isComplete
    , damage = ( toFloat sum.damage ) / size,
      teamValue = ( toFloat sum.teamValue ) / size
    }
