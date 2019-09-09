module French exposing (frenchTranslations)

import Translations exposing (..)

frenchTranslations : Translations
frenchTranslations =
  { notFound = "Introuvable"
  , authenticating = "Authentification en cours..."

  -- Global
  , appTitle = "E&P stats"

  -- Config
  , appKey = "Clef"
  , copy = "Copier!"
  , configureYourAlliance = "Configurez votre alliance"
  , copyTheKeysAndValidate = "Copiez les clefs et validez"
  , iveStoredThemAway = "J'les ai mises de côté"
  , invalidAppKey = "Clef invalide!"
  , pasteAppKey = "Collez votre clef"
  , adminKey = "Clef pour administrateurs"
  , peonKey = "Clef pour péons"
  , see = "Voir"
  , create = "Créer"

  -- Stats
  , alliance = "Alliance"
  , allianceMembers = "Membres de l'alliance"
  , titans = "Titans"
  , titanScores = "Scores sur titans"
  , averageTitanScore = "Score moyen sur titan"
  , preferredTitanColor = "Couleur de titan préférée"
  , wars = "Guerres"
  , warScores = "Scores de guerre"
  , averageWarScore = "Score moyen en guerre"
  , preferredWarBonus = "Bonus de guerre préféré"
  , pseudo = "Pseudo"
  , period = "Période"
  , color = "Couleur"
  , stars = "Etoiles"
  , bonus = "Bonus"
  , member = "Membre"
  , teamValue = "Valeur pour l'équipe"
  , filters = "Filtres"
  , fetchingTheData = "Récupération des données"
  , membersListsDiffer = "La liste de membres diffère entre les onglets titans et guerres"
  , keyRevoked = "La clef que vous utilisez a été révoquée. Demandez-en une nouvelle à votre administrateur."
  , backToAppKeyPage = "Utiliser une nouvelle clef"
  , red = "FEU"
  , green = "NATURE"
  , blue = "GLACE"
  , holy = "SACRE"
  , dark = "SOMBRE"
  , heal = "SOINS"
  , attack = "ATTAQUE"
  , arrows = "FLECHES"
  , all = "TOU(TE)S"

  -- Pages
  , appConfig= "Configuration"
  , appKeyCopy= "Copie de clef"
  , authorizationCallback = "Page d'authentification"
  , privacyPolicy = "Politique de confidentialité"
  , welcome = "Bienvenue"
  }
