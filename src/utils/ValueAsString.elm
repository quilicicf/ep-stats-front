module ValueAsString exposing (valueAsString)

import String.Interpolate exposing (interpolate)

valueAsString : String -> String
valueAsString value = interpolate "'{0}'" [ value ]
