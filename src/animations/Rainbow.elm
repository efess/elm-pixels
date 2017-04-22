module Animations.Rainbow exposing (getFrame) 

import PixelMatrix exposing (PixelMatrix, empty, adjustSize)
import Pixel exposing (Pixel)
import Rgb exposing (..)
import Time exposing (Time)
import Window
import Animations.PixelDimensions exposing (getDimensions)

pixelSize = 50
spectrumWidth = 255
partWidth = 170
initialState = { pixelWidth = 0, pixelHeight = 0, pixelTotal = 0 }

circularize: Int -> Int -> Int
circularize x max =
  abs (x % max)

calcColor : Int -> Int
calcColor x =
  round (sin ((toFloat x) * (pi / partWidth)) * 255)
  
-- step adder, step itself, color value
getColorAddPart : Int -> Int -> Int
getColorAddPart start x =
  calcColor (circularize (x + start) spectrumWidth)

getNextColor: Int -> Color
getNextColor x =
  let 
    circularX = x % 255
  in
    {
      r = getColorAddPart 0 x, 
      g = getColorAddPart 84 x, 
      b = getColorAddPart 170 x
    }

createPixel: Int -> Int -> Pixel
createPixel id x =
  let 
    color = getNextColor x
  in
    {
      backColor = color, 
      foreColor = color,
      glyph = ' ',
      id = id,
      x = 0,
      y = 0,
      width = pixelSize
    }


getFrame: Time -> Int -> Window.Size -> PixelMatrix
getFrame time frameNum windowSize =
    let
      dimensions = getDimensions windowSize pixelSize
      pixels = List.indexedMap createPixel (List.range frameNum ((+)frameNum dimensions.total))
    in
      PixelMatrix.listToMatrix pixels dimensions.width