module CreateBearerHeader exposing (createBearerHeader)

import Http exposing (..)

createBearerHeader : String -> Http.Header
createBearerHeader accessToken = Http.header "Authorization" ( "Bearer " ++ accessToken )
