module Pixel exposing (Pixel, emptyPixel)
import Rgb exposing (Color) 

type alias Pixel = {
  id: Int,
  x: Int,
  y: Int,
  width: Int,
  glyph: Char,
  backColor: Color,
  foreColor: Color
}

emptyPixel: Pixel
emptyPixel = 
  {
    id = 0,
    x = 0,
    y = 0,
    width = 0,
    glyph = ' ',
    backColor = {r = 0, g = 0, b = 0},
    foreColor = {r = 0, g = 0, b = 0}
  }