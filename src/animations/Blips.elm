module Animations.Blips exposing (getFrame, setup, BlipsState)
import Random exposing (Seed, int, step)
import Time exposing (Time)
import Pixel exposing (Pixel)
import Rgb exposing (..)

pixelStepGenerator = Random.int 15 10000

weridPeriodicFn: Float -> Float -> Float
weridPeriodicFn x stretch =
  (sin ((0.25 / stretch) * x) +
    sin ((0.52 / stretch) * x) +
    cos ((0.13 / stretch) * x) +
    sin ((0.29 / stretch) * x)) / 4

weirdPeriodic: Int -> Float
weirdPeriodic x =
  weridPeriodicFn (toFloat x) 30

sawtooth: Int -> Int
sawtooth x = 
  let
    value = x % 256
  in
    if x % 512 > 255 then 255 - value else value

type alias BlipsState = {
  color: Color,
  pixelOffsets: List Int
}

randomColorStep: Int -> List Int -> Seed -> {list: List Int, seed: Seed}
randomColorStep length list seed =
  let
    rndState = Random.step pixelStepGenerator seed
    next = {
      seed = snd rndState,
      list = fst rndState :: list
    }
    curLength = length - 1
  in
    if curLength == 0 then
      next
    else
      randomColorStep curLength next.list next.seed

setupState: Time -> Int -> BlipsState
setupState time numberUnits =
  let
    initalSeed = Random.initialSeed (round (Time.inMilliseconds time))
    rndColorRes = randomColorStep numberUnits [] initalSeed
  in
    {
      pixelOffsets = rndColorRes.list,
      color = { r = 0, g = 100, b = 100}
    }

alterState: Time -> Int -> BlipsState -> BlipsState
alterState time frameNum currenState =
  let
    green = round((weirdPeriodic frameNum) * 128) + 128
    color = currenState.color
    newState = { currenState | color = {r = color.r, g = green, b = color.b } }
  in
    newState

getNormalPixelColor: Color -> Int -> Int -> Color
getNormalPixelColor color frameNum offset =
  {
    r = color.r,
    g = round((weirdPeriodic (frameNum + offset)) * 128) + 128,--color.g,
    b = color.b
  }


createPixel: BlipsState -> Int -> Int -> Int -> Pixel
createPixel state frameNum id offset =
  {
    color = getNormalPixelColor state.color frameNum offset, 
    id = id
  }

getFrame: Time -> Int -> Int -> BlipsState -> (List Pixel, BlipsState)
getFrame time frameNum pixelCount state =
  let 
    newState = 
      if List.length state.pixelOffsets /= pixelCount then
        setupState time pixelCount
      else
        state -- alterState time frameNum state
  in
    (List.indexedMap (createPixel newState frameNum) newState.pixelOffsets, newState)


setup: BlipsState
setup = 
  { 
    color = { r = 0, g = 0, b = 0},
    pixelOffsets = []
  }