module English exposing (englishTranslations)

import Translations exposing (..)

englishTranslations : Translations
englishTranslations =
  { notFound = "Not found"
  , authenticating = "Authenticating..."

  -- Global
  , appTitle = "E&P stats"

  -- Config
  , appKey = "App key"
  , copy = "Copy!"
  , configureYourAlliance = "Configure your alliance"
  , copyTheKeyAndValidate = "Copy the key and validate"
  , iveStoredItAway = "I've stored it away"
  , invalidAppKey = "Invalid app key!"
  , pasteAppKey = "Paste your app key"
  , see = "See"
  , create = "Create"

  -- Stats
  , alliance = "Alliance"
  , allianceMembers = "Alliance members"
  , titans = "Titans"
  , titanScores = "Titans scores"
  , averageTitanScore = "Average titan score"
  , preferredTitanColor = "Preferred titan color"
  , wars = "Wars"
  , warScores = "War scores"
  , averageWarScore = "Average war score"
  , preferredWarBonus = "Preferred war bonus"
  , progression = "Progression"
  , pointsPerEvent = "pts/event"
  , participation = "Participation"
  , member = "Member"
  , pseudo = "Pseudo"
  , period = "Period"
  , color = "Color"
  , stars = "Stars"
  , bonus = "Bonus"
  , teamValue = "Team value"
  , filters = "Filters"
  , fetchingTheData = "Fetching the data"
  , membersListsDiffer = "The members lists differ in the Titans and Wars tabs, contact your statistician!"
  , backToAppKeyPage = "Use another key"
  , red = "FIRE"
  , green = "NATURE"
  , blue = "ICE"
  , holy = "HOLY"
  , dark = "DARK"
  , heal = "HEAL"
  , attack = "ATTACK"
  , arrows = "ARROWS"
  , all = "ALL"

  -- Pages
  , appConfig= "App config"
  , appKeyCopy= "App key copy"
  , authorizationCallback = "Authorization callback"
  , privacyPolicy = "Privacy policy"
  , welcome = "Welcome"
  }
