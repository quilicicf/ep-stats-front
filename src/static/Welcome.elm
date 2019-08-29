module Welcome exposing (viewWelcome)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Msg exposing (..)
import Constants exposing (..)
import Internationalization exposing (Language(..))

viewWelcome : Language -> Html Msg
viewWelcome language =
  case language of
    French ->
      div [ class "welcome" ] [
        div [ class "textual-content" ] [
          h2 [] [ text "Bienvenue sur Empires&Puzzles stats!" ],
          p [] [
            text "Ce site est conçu pour aider les alliances de ",
            empiresAndPuzzlesLink,
            text " à s'améliorer. Il présente des statistiques sur l'alliance et ses membres."
          ],
          p [] [
            text "Ce site est développé sur mon temps personnel pour le fun. Je ne suis lié à Small Giant (développeur du jeu) d'aucune façon."
          ],
          p [] [
            text "Ce site récupère les statistique de jeu sur une Google Sheet qui doit être maintenu séparément par les membres de l'alliance."
          ],
          p [] [
            text "Vous pouvez trouver des instructions (en Anglais uniquement) pour mettre le système en place sur ",
            a [ href epStatsFrontRepositoryUrl ] [ text "le dépôt contenant le code (open-source) de l'application" ],
            text "."
          ],
          div [ class "start-using" ] [
            button [ class "button button-secondary", onClick ( WelcomeMsg <| ICreateKey ) ] [ text "Je veux créer ma clef"],
            button [ class "button button-primary", onClick ( WelcomeMsg <| IHasKey ) ] [ text "On m'a donné une clef" ]
          ]
        ]
      ]
    English ->
      div [ class "welcome" ] [
        div [ class "textual-content" ] [
          h2 [] [ text "Welcome to Empires&Puzzles stats!" ],
          p [] [
            text "This website is meant to help ",
            empiresAndPuzzlesLink,
            text " alliances improve by showing stats of their performance."
          ],
          p [] [
            text "This website is developed on my free time for fun, I'm not affiliated to Small Giant (the game's developer) in any way."
          ],
          p [] [
            text "This website retrieves its data from a Google Sheet that must be maintained by alliance members."
          ],
          p [] [
            text "You can find instructions to bootstrap things on ",
            a [ href epStatsFrontRepositoryUrl ] [ text "the website's open-source code repository" ],
            text "."
          ],
          div [ class "start-using" ] [
            button [ class "button button-secondary", onClick ( WelcomeMsg <| ICreateKey ) ] [ text "I will create the app key"],
            button [ class "button button-primary", onClick ( WelcomeMsg <| IHasKey ) ] [ text "I have an app key" ]
          ]
        ]
      ]
