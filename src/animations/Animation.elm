module Animations.Animation exposing (AniState, AniResult, runFrame, setup)
import Animations.Rainbow
import Animations.Blips
import Animations.Matrix
import PixelMatrix exposing (PixelMatrix)
import Time exposing (Time)
import Window

type alias AniResult = {
  pixelMatrix: PixelMatrix,
  state: AniState
}

type AniState = 
  AniBlipsState Animations.Blips.BlipsState
  | AniMatrixState Animations.Matrix.MatrixState
  | Rainbow

createAniResult: PixelMatrix -> AniState -> AniResult
createAniResult pixelMatrix state =
  {
    pixelMatrix = pixelMatrix,
    state = state
  }

-- HOW DO YOU DO THIS BETTER
runFrame: Time -> Int -> Window.Size -> AniState -> AniResult
runFrame time frameNum viewSize aniState =
  case aniState of
    Rainbow ->
        createAniResult (Animations.Rainbow.getFrame time frameNum viewSize) Rainbow
    AniBlipsState state ->
      let 
        tuple = Animations.Blips.getFrame time frameNum viewSize state
      in
        createAniResult (Tuple.first tuple) (AniBlipsState (Tuple.second tuple))
    AniMatrixState state ->
      let 
        tuple = Animations.Matrix.getFrame time frameNum viewSize state
      in
        createAniResult (Tuple.first tuple) (AniMatrixState (Tuple.second tuple))
      
-- Setup a new animation
setup: String -> Window.Size -> AniState
setup name viewPort =
  case name of
    "rainbow" -> Rainbow
    "blips"   -> AniBlipsState (Animations.Blips.setup viewPort)
    "matrix"  -> AniMatrixState (Animations.Matrix.setup viewPort)
    _         -> Rainbow