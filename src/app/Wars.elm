module Wars exposing (WarBonus(..), WarBonusData, warBonusesByCode, warBonusFromString, getWarBonusData)

import AssocList as Dict exposing (Dict)
import FlipTuple exposing (flipTuple)
import Translations exposing (..)

type alias WarBonusData =
  { code : String -- The code (to compare with the gsheet)
  , name : String -- The displayable (internationalized) name, i.e. HEAL
  , icon : String -- The CSS class for the appropriate icon
  }

type WarBonus = HealBonus | AttackBonus | ArrowsBonus | AllBonus

warBonusFromString : String -> WarBonus
warBonusFromString warBonusAsString = Dict.get warBonusAsString warBonusesByCode |> Maybe.withDefault AllBonus

getWarBonusData : Translations -> WarBonus -> WarBonusData
getWarBonusData translations warBonus = case warBonus of
  HealBonus -> WarBonusData "HEAL" translations.heal "fas fa-heartbeat"
  AttackBonus -> WarBonusData "ATTACK" translations.attack "fas fa-arrow-up"
  ArrowsBonus -> WarBonusData "ARROWS" translations.arrows "fas fa-bullseye"
  AllBonus -> WarBonusData "ALL" translations.all "fas fa-question"

warBonusesByCode : Dict String WarBonus
warBonusesByCode = Dict.fromList [
    ( "ALL", AllBonus ),
    ( "HEAL", HealBonus ),
    ( "ATTACK", AttackBonus ),
    ( "ARROWS", ArrowsBonus)
  ]
