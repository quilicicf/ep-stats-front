module MemberScore exposing (MemberScore, AverageMemberScore)

type alias MemberScore =
  { damage : Int
  , teamValue : Int
  }

type alias AverageMemberScore =
  { isComplete : Bool
  , damage : Float
  , teamValue : Float
  }
