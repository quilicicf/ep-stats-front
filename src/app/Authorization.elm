module Authorization exposing (makeAuthorizationUrl, readAccessToken)

import Dict exposing (..)
import Maybe exposing (..)
import OAuth.Implicit exposing (..)
import Url exposing (..)
import Url.Parser as Parser exposing (..)

import GetAt exposing (..)

makeAuthorization : Url -> Authorization
makeAuthorization baseUrl = Authorization
  "1011659939807-9eit2lra1p046chbhuv1be6pttloctbm.apps.googleusercontent.com"
  ( Url Url.Https "accounts.google.com" Nothing "/o/oauth2/v2/auth" Nothing Nothing )
  { baseUrl | path = "/authorized" }
  [ "https://www.googleapis.com/auth/spreadsheets.readonly", "https://www.googleapis.com/auth/drive.metadata.readonly" ]
  ( Just "dodelidoo" ) -- TODO: Randomize?

makeAuthorizationUrl : Url -> String
makeAuthorizationUrl baseUrl = toString ( makeAuthUrl ( makeAuthorization baseUrl ) )

parseArgument : String -> Dict String String -> Dict String String
parseArgument parameterAsString seed =
  let
    nameAndValue : List String
    nameAndValue = String.split "=" parameterAsString

    name : String
    name = withDefault "" ( getAt 0 nameAndValue )

    value : String
    value = withDefault "" ( getAt 1 nameAndValue )

  in
    Dict.insert name value seed

parseFragment : String -> Dict String String
parseFragment fragment =
  let
    parametersAsString : List String
    parametersAsString = String.split "&" fragment

  in
    List.foldl parseArgument Dict.empty parametersAsString

parsePotentialFragment : Maybe String -> String
parsePotentialFragment maybeFragment =
  case maybeFragment of
    Just fragment -> fragment
    Nothing -> ""

readAccessToken : Url -> Maybe String
readAccessToken url =
  let
    urlFragment : String
    urlFragment = withDefault "" ( parse (Parser.s "authorized" </> fragment parsePotentialFragment) url )

    parsedFragment : Dict String String
    parsedFragment = parseFragment urlFragment
  in
    Dict.get "access_token" parsedFragment -- TODO: parse state
