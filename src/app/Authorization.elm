module Authorization exposing (authorizationUrl, readAccessToken)

import Dict exposing (..)

import List.Extra exposing (getAt)

import Maybe exposing (withDefault)

import OAuth.Implicit exposing (Authorization, makeAuthUrl)

import Url exposing (..)
import Url.Parser as Parser exposing (..)

import MaybeExtra exposing (withLoggedDefault)

authorization : Authorization
authorization = Authorization
  "1011659939807-qe1bito30nlfd03lp8tosvgmsgacrns1.apps.googleusercontent.com"
  ( Url Url.Https "accounts.google.com" Nothing "/o/oauth2/v2/auth" Nothing Nothing )
  ( Url Url.Https "localhost" ( Just 5420 ) "/authorized" Nothing Nothing ) -- TODO: variabelize
  [ "https://www.googleapis.com/auth/spreadsheets.readonly" ]
  ( Just "dodelidoo" ) -- TODO: Randomize?

authorizationUrl : String
authorizationUrl = toString ( makeAuthUrl authorization )

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
    urlFragment = withLoggedDefault "" ( parse (Parser.s "authorized" </> fragment parsePotentialFragment) url )

    parsedFragment : Dict String String
    parsedFragment = parseFragment urlFragment
  in
    Dict.get "access_token" parsedFragment -- TODO: parse state
