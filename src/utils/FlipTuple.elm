module FlipTuple exposing (flipTuple)

flipTuple : ( a, b ) -> ( b, a )
flipTuple tuple = (Tuple.second tuple, Tuple.first tuple)
