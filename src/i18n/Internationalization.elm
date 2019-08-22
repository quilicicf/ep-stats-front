module Internationalization exposing (Language(..), languages, findLanguage, languageToString, getTranslations)

import Translations exposing (Translations)

import French exposing (frenchTranslations)
import English exposing (englishTranslations)

type Language = English | French

languages : List ( String, Language )
languages =
  [ ( "en", English )
  , ( "fr", French )
  ]

defaultLanguage : ( String, Language )
defaultLanguage = ( "en", English )

languageToString : Language -> String
languageToString languageToStringify = List.filter (\(_, language) -> language == languageToStringify) languages
  |> List.head
  |> Maybe.withDefault defaultLanguage
  |> Tuple.first

findLanguage : String -> Language
findLanguage languageToParse = List.filter (\(languageAsString, _) -> String.startsWith languageAsString languageToParse) languages
 |> List.head
 |> Maybe.withDefault defaultLanguage
 |> Tuple.second

getTranslations : Language -> Translations
getTranslations language =
  case language of
    French -> frenchTranslations
    English -> englishTranslations
