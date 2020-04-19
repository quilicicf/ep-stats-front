module Gsheet exposing (
  RawStats, RawSheet,
  fixedTitanIndexes, fixedWarIndexes,
  computeSheetDataUrl, decodeRawStats
  )

import Json.Decode as Decode exposing(Value, Decoder, string)
import Json.Decode.Pipeline exposing (..)
import Url exposing (..)

import CreateQueryString exposing (..)

------------
-- MODELS --
------------

type alias RawStats = { valueRanges : List RawSheet }

type alias RawSheet = { values: List ( List String ) }

---------------
-- CONSTANTS --
---------------

fixedTitanIndexes =
  { number = 6
  , dateIndex = 0
  , totalIndex = 1
  , lifeIndex = 2
  , starIndex = 3
  , colorIndex = 4
  , membersIndex = 5
  }

fixedWarIndexes =
  { number = 5
  , dateIndex = 0
  , totalIndex = 1
  , enemyScoreIndex = 2
  , bonusIndex = 3
  , membersIndex = 4
  }

-------------
-- METHODS --
-------------

rawSheetStatsDecoder : Decoder RawSheet
rawSheetStatsDecoder =
  Decode.succeed RawSheet
   |> required "values" ( Decode.list (Decode.list string) )

rawStatsDecoder : Decoder RawStats
rawStatsDecoder =
  Decode.succeed RawStats
    |> required "valueRanges" ( Decode.list rawSheetStatsDecoder )

decodeRawStats : String -> RawStats
decodeRawStats statsAsString =
  let
    decodingResult : Result Decode.Error RawStats
    decodingResult = Decode.decodeString rawStatsDecoder statsAsString

  in
    case decodingResult of
      Ok rawStats -> rawStats
      Err _ -> RawStats []


batchGetQueryString : List ( String, String )
batchGetQueryString = [
    ( "ranges", "Titans!A:AAZ" ),
    ( "ranges", "Wars!A:AAZ" ),
    ( "valueRenderOption", "UNFORMATTED_VALUE" )
  ]

computeSheetDataUrl : String -> String
computeSheetDataUrl sheetId =
  let
    url : Url
    url = Url
      Url.Https
      "sheets.googleapis.com"
      Nothing -- Port
      ( String.join "/" [ "", "v4", "spreadsheets", sheetId, "values:batchGet" ] ) -- Path
      ( Just ( createQueryString batchGetQueryString ) ) -- Query
      Nothing -- Fragment
  in
    Url.toString url
