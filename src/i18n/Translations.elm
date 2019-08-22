module Translations exposing (Translations, TranslationsExtender)

type alias Translations =
  { notFound : String
  , authenticating: String

  -- Global
  , appTitle: String

  -- Config
  , appKey: String
  , copy: String
  , configureYourAlliance: String
  , copyTheKeysAndValidate: String
  , iveStoredThemAway: String
  , invalidAppKey: String
  , pasteAppKey: String
  , adminKey: String
  , peonKey: String
  , see: String
  , create: String

  -- Stats
  , alliance: String
  , allianceMembers: String
  , titans: String
  , titanScores: String
  , averageTitanScore: String
  , preferredTitanColor: String
  , wars: String
  , warScores: String
  , averageWarScore: String
  , preferredWarBonus: String
  , member: String
  , pseudo: String
  , period: String
  , color: String
  , stars: String
  , bonus: String
  , teamValue: String
  , filters: String
  , fetchingTheData: String
  , membersListsDiffer: String
  , red: String
  , green: String
  , blue: String
  , holy: String
  , dark: String
  , heal: String
  , attack: String
  , arrows: String
  , all: String

  -- Pages
  , appConfig: String
  , appKeyCopy: String
  , authorizationCallback: String
  }

type alias TranslationsExtender r = { r | translations: Translations }
