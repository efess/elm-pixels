module Animations.Blips exposing (getFrame, setup, BlipsState)
import Random exposing (Seed, int, step)
import Maybe
import Time exposing (Time)
import Pixel exposing (Pixel)
import PixelMatrix exposing (PixelMatrix, empty)
import Rgb exposing (..)
import Array exposing (..)
import Window
import Matrix exposing (..)
import Animations.PixelDimensions exposing (PixelDimensions, getDimensions)

pixelSize = 60
pixelStepGenerator = Random.int 15 10000
pixelColorGenerator = Random.int 0 5

colorPallet = Array.fromList [
  { r = 32, g = 89, b = 19 },-- green
  { r = 193, g = 89, b = 9 }, -- orange
  { r = 163, g = 9, b = 12 }, -- red
  { r = 9, g = 30, b = 163 }, -- blue
  { r = 193, g = 19, b = 193 }]


weridPeriodicFn: Float -> Float -> Float
weridPeriodicFn x stretch =
  (sin ((0.25 / stretch) * x) +
    sin ((0.52 / stretch) * x) +
    cos ((0.13 / stretch) * x) +
    sin ((0.29 / stretch) * x)) / 4

weirdPeriodic: Int -> Float
weirdPeriodic x =
  weridPeriodicFn (toFloat x) 50

sawtooth: Int -> Int
sawtooth x = 
  let
    value = x % 256
  in
    if x % 512 > 255 then 255 - value else value

type alias PixelParams = {
  color: Color,
  offset: Int
}

type alias BlipsState = {
  pixelParams: Matrix PixelParams,
  dimensions: PixelDimensions
}

numberToColor: Int -> Color
numberToColor x =
  Array.get x colorPallet
    |> Maybe.withDefault { r = 193, g = 89, b = 9 }

emptyParams: PixelParams
emptyParams = {color= { r = 0, g = 100, b = 100}, offset = 0}

emptyParamsMatrix: Int -> Int -> Matrix PixelParams
emptyParamsMatrix width height = Matrix.matrix height width (\_ -> emptyParams)

expandParamsMatrix: Int -> Int -> Matrix PixelParams -> Matrix PixelParams
expandParamsMatrix width height matrix =
  Matrix.matrix height width (\l -> Maybe.withDefault emptyParams (Matrix.get l matrix))

randomPixelParam: Seed -> {pixelParam: PixelParams, seed: Seed}
randomPixelParam seed =
  let
    rndColor = Random.step pixelColorGenerator seed
    rndOffset = Tuple.second rndColor 
      |> Random.step pixelStepGenerator 
  in
    {
      pixelParam = {
        color = Tuple.first rndColor
          |> numberToColor,
        offset = Tuple.first rndOffset
      },
      seed = Tuple.second rndOffset
    }

fillExpandedPixelParmas: PixelDimensions -> Int -> Int -> Matrix PixelParams -> Seed -> {result: Matrix PixelParams, seed: Seed}
fillExpandedPixelParmas prevSize index total matrix seed =
  let
    x = index % colCount matrix
    y = floor (toFloat index / toFloat (colCount matrix))
    nextPixelParam = randomPixelParam seed
    next = {
      result = if x >= prevSize.width || y >= prevSize.height 
        then Matrix.set (loc y x) nextPixelParam.pixelParam matrix 
        else matrix,
      seed = nextPixelParam.seed
    }
  in
    if index == total then
      next
    else
      fillExpandedPixelParmas prevSize (index + 1) total next.result next.seed

updateState: Time -> BlipsState -> PixelDimensions -> BlipsState
updateState time currentState newDimensions =
  let
    initalSeed = Time.inMilliseconds time
      |> round
      |> Random.initialSeed
    expandedMatrix = expandParamsMatrix newDimensions.width newDimensions.height currentState.pixelParams
    pixelParams = fillExpandedPixelParmas currentState.dimensions 0 newDimensions.total expandedMatrix initalSeed
  in
    {
      pixelParams = pixelParams.result,
      dimensions = newDimensions
    }

getNormalPixelColor: Color -> Int -> Int -> Color
getNormalPixelColor color frameNum offset =
  let 
    greenBlue = { r = 0, g = 100, b = 100}
    alterAmount = round(150 * weirdPeriodic (frameNum + offset))
  in
    {
      r = greenBlue.r,--  + alterAmount,
      g = greenBlue.g  + alterAmount,--color.g,
      b = greenBlue.b--  + alterAmount
    }

createPixel: BlipsState -> Int -> Location -> PixelParams -> Pixel
createPixel state frameNum location pixelParams =
  let 
    color = getNormalPixelColor pixelParams.color frameNum pixelParams.offset
  in
    {
      backColor = color, 
      foreColor = color,
      glyph = ' ',
      id = (row location * state.dimensions.width) + col location,
      x = 0,
      y = 0,
      width = pixelSize
    }

getFrame: Time -> Int -> Window.Size -> BlipsState -> (PixelMatrix, BlipsState)
getFrame time frameNum windowSize state =
  let 
    dimensions = getDimensions windowSize pixelSize
    newState = 
      if state.dimensions.total /= dimensions.total then
        updateState time state dimensions -- update only if dimensions change
      else
        state
  in
    (
      Matrix.mapWithLocation (createPixel newState frameNum) state.pixelParams, 
      newState
    )

setup: Window.Size -> BlipsState
setup viewSize = 
  { 
    dimensions = { height = 0, total = 0, width = 0},
    pixelParams = emptyParamsMatrix 0 0
  }