module Animations.Rainbow exposing (getFrame) 

import Pixel exposing (Pixel)
import Rgb exposing (..)

spectrumWidth = 255
partWidth = 170

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
  {color = getNextColor x, id = id}

getFrame: Int -> Int -> List Pixel
getFrame frameNum pixelCount =
   List.indexedMap createPixel [frameNum..((+)frameNum pixelCount)]
