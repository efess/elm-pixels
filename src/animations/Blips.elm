module Animations.Blips exposing (getFrame, setup, BlipsState) 
--import Animations.AnimationState exposing (AniState, AniResult)
import Pixel exposing (Pixel)
import Rgb exposing (..)

spectrumWidth = 255

getNextColor: Int -> Color
getNextColor x =
  {
    r = 0,
    b = 0,
    g = 255
  }

type alias BlipsState = {
  color: Color
} 

-- empty slate

-- 

alterState: Int -> BlipsState -> BlipsState
alterState frameNum currenState =
  let
    value = frameNum % 256
    green = if frameNum % 512 > 255 then 255 - value else value
    color = currenState.color
    newState = { currenState | color = {r = color.r, g = green, b = color.b } }
  in
    newState

createPixel: BlipsState -> Int -> Int -> Pixel
createPixel state id x =
  {color = state.color, id = id}

getFrame: Int -> Int -> BlipsState -> (List Pixel, BlipsState)
getFrame frameNum pixelCount state =
  let 
    newState = alterState frameNum state
  in
   (List.indexedMap (createPixel newState) [frameNum..((+)frameNum pixelCount)], newState)

setup: BlipsState
setup = 
  { 
    color = { r = 0, g = 255, b = 100}
  }