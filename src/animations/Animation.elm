module Animations.Animation exposing (AniState, AniResult, runFrame, setup)
import Animations.Rainbow
import Animations.Blips
import Pixel exposing (Pixel)
import Time exposing (Time)

type alias AniResult = {
  pixels: List Pixel,
  state: AniState
}

type AniState = 
  AniBlipsState Animations.Blips.BlipsState 
  | Rainbow

createAniResult: List Pixel -> AniState -> AniResult
createAniResult pixels state =
  {
    pixels = pixels,
    state = state
  }

-- HOW DO YOU DO THIS BETTER
runFrame: Time -> Int -> Int -> AniState -> AniResult
runFrame time frameNum pixelCount aniState =
  case aniState of
    Rainbow ->
        createAniResult (Animations.Rainbow.getFrame time frameNum pixelCount) Rainbow
    AniBlipsState state ->
      let 
        tuple = Animations.Blips.getFrame time frameNum pixelCount state
      in
        createAniResult (fst tuple) (AniBlipsState (snd tuple))  
      
-- Setup a new animation
setup: String -> AniState
setup name =
  case name of
    "rainbow" -> Rainbow
    "blips"   -> AniBlipsState Animations.Blips.setup
    _         -> Rainbow