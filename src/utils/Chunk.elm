module Chunk exposing (chunk)

import List exposing (..)

chunk : Int -> List a -> List (List a)
chunk chunkSize listToChunk =
  let len = length listToChunk
  in  if len > chunkSize
      then take chunkSize listToChunk :: chunk chunkSize (drop chunkSize listToChunk)
      else [listToChunk]
