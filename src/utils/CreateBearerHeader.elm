module CreateBearerHeader exposing (createBearerHeader)

import Http exposing (..)

import String.Interpolate exposing (interpolate)

createBearerHeader : String -> Http.Header
createBearerHeader accessToken = Http.header "Authorization" ( interpolate "Bearer {0}" [ accessToken ] )
