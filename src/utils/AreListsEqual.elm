module AreListsEqual exposing (areListsEqual)

accumulator : List comparable -> comparable -> Bool -> Bool
accumulator listB item seed =
  seed && ( List.member item listB )

areListsEqual : List comparable -> List comparable -> Bool
areListsEqual listA listB =
  ( List.length listA == List.length listB )
    && List.foldl ( accumulator listB ) True listA
