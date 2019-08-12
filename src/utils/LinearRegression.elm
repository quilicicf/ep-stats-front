module LinearRegression exposing (RegressionResult, computeLinearRegression)

import Basics exposing (round)

import Flip exposing (flip)
import GetAt exposing (getAt)

type alias RegressionOptions =
  { order: Int
  , precision: Int
  }

type alias RegressionResult =
  { gradient: Float
  , intercept: Float
  , predict: Int -> Float
  }

type alias Matrix =
  { length: Int
  , accu1: Int
  , accu2: Int
  , accu3: Int
  , accu4: Int
  , accu5: Int
  }

roundWithPrecision : Int -> Float -> Float
roundWithPrecision precision number =
  let
    coeff : Float
    coeff = List.repeat precision "0"
      |> String.join ""
      |> (++) "1"
      |> String.toFloat
      |> Maybe.withDefault 1.0
  in
    number * coeff
      |> round
      |> toFloat
      |> flip (/) coeff

computeLinearRegression : Int -> List ( List Int ) -> RegressionResult
computeLinearRegression precision data =
  let
    matrix : Matrix
    matrix = List.foldl addDataToSum ( Matrix 0 0 0 0 0 0 ) data

    run : Int
    run = ((matrix.length * matrix.accu3) - (matrix.accu1 * matrix.accu1))

    rise : Int
    rise = ((matrix.length * matrix.accu4) - (matrix.accu1 * matrix.accu2))

    gradient : Float
    gradient = if run == 0
      then 0
      else (roundWithPrecision precision ( (toFloat rise) / (toFloat run) ))

    intercept : Float
    intercept = roundWithPrecision precision (toFloat matrix.accu2 / toFloat matrix.length) - ((gradient * toFloat matrix.accu1) / toFloat matrix.length)

    predict : Int -> Float
    predict x = roundWithPrecision precision ((gradient * toFloat x) + intercept)

  in
    RegressionResult gradient intercept predict


addDataToSum : List Int -> Matrix -> Matrix
addDataToSum dataPoint matrix =
  let
    maybePoint : Maybe ( Int, Int )
    maybePoint = Maybe.map2 (\abscissa ordinate -> (abscissa, ordinate) ) (getAt 0 dataPoint) (getAt 1 dataPoint)
  in
    case maybePoint of
      Nothing -> matrix
      Just (abscissa, ordinate) -> Matrix
        (matrix.length + 1)
        (matrix.accu1 + abscissa)
        (matrix.accu2 + ordinate)
        (matrix.accu3 + ( abscissa * abscissa ))
        (matrix.accu4 + ( abscissa * ordinate ))
        (matrix.accu5 + ( ordinate * ordinate ))
