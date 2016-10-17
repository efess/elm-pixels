module Animations.Animation exposing (AniState, AniResult, runFrame, setup)
import Animations.Rainbow
import Animations.Blips
import Pixel exposing (Pixel)

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
runFrame: Int -> Int -> AniState -> AniResult
runFrame frameNum pixelCount aniState =
  case aniState of
    Rainbow ->
      let 
        pixels = Animations.Rainbow.getFrame frameNum pixelCount
      in
        createAniResult pixels Rainbow
    AniBlipsState state ->
      let 
        tuple = Animations.Blips.getFrame frameNum pixelCount state
      in
        createAniResult (fst tuple) (AniBlipsState (snd tuple))  
      
-- Setup a new animation
setup: String -> AniState
setup name =
  case name of
    "rainbow" -> Rainbow
    "blips"   -> AniBlipsState Animations.Blips.setup
    _         -> Rainbow