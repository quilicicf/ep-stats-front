module PrivacyPolicy exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

viewPrivacyPolicy : Html msg
viewPrivacyPolicy =
  div [ class "privacy-policy" ] [
    div [ class "textual-content" ] [
      p [] [ text """
        This Privacy Policy describes how and when I collect, use, and share information when you use my website: ep-stats.netlify.com.
        """
      ],
      p [] [ text """
        This Privacy Policy does not apply to the practices of third parties that I do not own or control, including any third party services you access through E&P stats.
        """
      ],

      h2 [ class "title" ] [ text "1. Information I collect" ],
      p [] [ text "To show you your stats, you must provide me with an access to the Google Sheet where they are stored." ],
      p [] [ text "For that purpose, you must authorized E&P stats to read the Google Sheets your Google account has read (or more) access to." ],
      p [] [ text "None of the information collection is personally identifying." ],

      h2 [ class "title" ] [ text "2. Why I need your information and how I use it" ],
      p [] [ text "Only the strict necessary information is used:" ],
      ul [] [
        li [] [ text "An access token to read the data in your Google sheet" ],
        li [] [ text "The id of your Google Sheet" ],
        li [] [ text "The name of your E&P alliance" ],
        li [] [ text "The data from your Google Sheet" ]
      ],
    p [] [ text "The access token, Google sheet id and E&P alliance name are stored on your machine only to avoid re-typing them each visit." ],
    p [] [ text "The data from your Google Sheet is only kept until the page is closed." ],
    p [] [ text "None of this is sent anywhere, your data stays on your computer." ],

    h2 [ class "title" ] [ text "3. Information sharing" ],
    p [] [ text "Your data is not shared with anyone." ],

    h2 [ class "title" ] [ text "4. Data retention" ],
    p [] [ text "Your data is not shared with anyone, which implies no-one stores it anywhere." ],

    h2 [ class "title" ] [ text "4. Transfers of Personal Information Outside the EU" ],
    p [] [ text "Your data is not shared with anyone, it can't leave the EU." ],

    h2 [ class "title" ] [ text "5. Revoke E&P access to my data" ],
    p [] [
      span [] [ text "You can always revoke E&P stats's access to your data by going to " ],
      a [ href "https://myaccount.google.com/permissions" ] [ text "your Google account's permissions page" ],
      span [] [ text " and deleting access to the app." ]
    ],

     h2 [ class "title" ] [ text "5. How to contact me" ],
     p [] [ text "You can contact me at leplusfortcestgaston@gmail.com" ]
    ]
  ]
