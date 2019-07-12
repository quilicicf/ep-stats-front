module MemberScore exposing (MemberScore, AverageMemberScore)

type alias MemberScore =
  { damage : Int
  , teamValue : Int
  }

type alias AverageMemberScore =
  { damage : Float
  , teamValue : Float
  }
