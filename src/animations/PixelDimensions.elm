module Animations.PixelDimensions exposing (PixelDimensions, getDimensions)
import Window

type alias PixelDimensions = {
  total: Int,
  width: Int,
  height: Int
}

getDimensions: Window.Size -> Int -> PixelDimensions
getDimensions viewSize pixelSize =
  let
    pixelWidth = floor (toFloat viewSize.width / toFloat  pixelSize) + 1 -- overlap window edge
    pixelHeight = floor (toFloat viewSize.height / toFloat  pixelSize) + 1
  in
    { 
      width = pixelWidth,
      height = pixelHeight,
      total = pixelWidth * pixelHeight
    }