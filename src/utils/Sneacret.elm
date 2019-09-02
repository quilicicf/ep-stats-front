module Sneacret exposing (sneak, unSneak)

import Dict exposing (..)

import Flip exposing (..)
import Chunk exposing (..)
import GetAt exposing (..)
import Last exposing (..)
import MaybeExtra exposing (..)

invisibleSpaces : List Char
invisibleSpaces =
  [ '\u{200B}' -- zeroWidthSpace
  , '\u{200C}' -- zeroWidthNonJoiner
  , '\u{200D}' -- zeroWidthJoiner
  , '\u{2060}' -- wordJoiner
  , '\u{FEFF}' -- zeroWidthNoBreakSpace
  ]

indexedInvisibleSpaces : Dict Char Int
indexedInvisibleSpaces = List.indexedMap (\index space -> (space, index)) invisibleSpaces |> Dict.fromList

encodingBase : Int
encodingBase = 5 -- Number of different spaces

encodingCharacterSize : Int
encodingCharacterSize = 3 -- The smallest power of encodingBase that is above alphabet's size

alphabet : List Char
alphabet =
  [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
  , 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
  , 'é', 'è', 'ê', 'ë', 'î', 'à'
  , '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
  , '_', '-', '.', ':', ',', '{', '}', '[', ']', '(', ')', ' ', '"'
  ]

indexedAlphabet : Dict Char Int
indexedAlphabet = List.indexedMap (\index character -> (character, index)) alphabet |> Dict.fromList

padToEncodingCharacterSize : List Int -> List Int
padToEncodingCharacterSize currentValue =
  let
    currentLength : Int
    currentLength = List.length currentValue

    missingLength : Int
    missingLength = encodingCharacterSize - currentLength
  in
    List.append ( List.repeat missingLength 0 ) currentValue

recursiveDowngradeRadix : Int -> Int -> List Int -> List Int
recursiveDowngradeRadix code radix2 currentResult =
  if code == 0
    then padToEncodingCharacterSize currentResult
    else recursiveDowngradeRadix ( code // radix2 ) radix2 ( ( modBy radix2 code ) :: currentResult )

recursiveUpgradeRadix : Int -> Int -> List Int -> Int -> Int
recursiveUpgradeRadix oldRadix index currentValue accumulator =
  let
    addingFactor : Int
    addingFactor = oldRadix ^ index
  in
    if List.isEmpty currentValue
      then accumulator
      else recursiveUpgradeRadix
        oldRadix
        ( index + 1 )
        ( List.tail currentValue |> Maybe.withDefault [] )
        ( accumulator + ( List.head currentValue |> Maybe.withDefault 0 |> (*) addingFactor ) )

encodeCharacter : Int -> String
encodeCharacter characterCode = recursiveDowngradeRadix characterCode encodingBase []
  |> List.map ( flip getAt invisibleSpaces )
  |> List.map ( Maybe.withDefault 'X' ) -- Can't happen
  |> String.fromList

encodeString : String -> String
encodeString initialText = String.toList initialText
  |> List.map ( flip Dict.get indexedAlphabet )
  |> List.map ( Maybe.withDefault 0 )
  |> List.map ( encodeCharacter )
  |> String.join ""

isCharacterAllowed : Char -> Bool -> Bool
isCharacterAllowed character currentResult =
  if currentResult == False
    then False
    else Dict.get character indexedAlphabet |> MaybeExtra.hasValue

isAllowed : String -> Bool
isAllowed value = String.toList value |> List.foldl isCharacterAllowed True

sneak : String -> String -> String -> Maybe String
sneak prefix suffix value =  if isAllowed value
  then Just ( prefix ++ ( encodeString value ) ++ suffix )
  else Nothing

decodeCharacter : List Char -> Char
decodeCharacter invisibles = List.map ( flip Dict.get indexedInvisibleSpaces ) invisibles
  |> List.map ( Maybe.withDefault 0 ) -- Can't happen because the inputs are filtered first
  |> (\codes -> recursiveUpgradeRadix encodingBase 0 ( List.reverse codes ) 0 )
  |> flip getAt alphabet
  |> Maybe.withDefault 'X' -- Can only happen for keys created manually with character codes overflowing alphabet size

unSneak : String -> Maybe String
unSneak value =
  let
    chunks : List (List Char)
    chunks = String.toList value
     |> List.filter ( flip List.member invisibleSpaces )
     |> chunk encodingCharacterSize

    lastChunkSize : Int
    lastChunkSize = last chunks |> Maybe.map List.length |> Maybe.withDefault 0

    hasRightNumberOfInvisibleSpaces : Bool
    hasRightNumberOfInvisibleSpaces = ( not <| List.isEmpty chunks ) && ( lastChunkSize == encodingCharacterSize )
  in
    if hasRightNumberOfInvisibleSpaces
      then List.map decodeCharacter chunks |> String.fromList |> Just
      else Nothing
