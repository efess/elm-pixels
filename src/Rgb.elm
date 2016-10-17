module Rgb exposing (..)

import Array
import String
import Maybe
import Bitwise exposing (..)

-- type ColorType = Red | Blue | Green

-- let propOfColor = function
--   | Red   -> "r"
--   | Blue  -> "b"
--   | Green -> "g"

type alias Color = {
  r : Int,
  g : Int,
  b : Int
}

hexArray = Array.fromList ["0", "1","2","3","4","5","6","7","8","9", "A","B", "C", "D", "E", "F"]

digitToHex: Int -> String
digitToHex x =
  Maybe.withDefault "0" (Array.get x hexArray) 

pullNextHex: Int -> List String -> List String
pullNextHex val acc =
  let 
    leftover = shiftRight val 4
    cur = val % 16
  in
    if leftover > 0 then 
      pullNextHex leftover (digitToHex cur :: acc) 
    else 
      (digitToHex cur :: acc) 

numToHex : Int -> String
numToHex num =
  String.padLeft 2 '0' (String.concat (pullNextHex num []))

toHex : Color -> String
toHex color =
   (numToHex color.r) ++ (numToHex color.g) ++ (numToHex color.b)

toInt : Color -> Int
toInt color = 
  or (shiftLeft color.r 16)  (or (shiftLeft color.g 8) color.b)

toColor : Int -> Color
toColor number = 
  {
    r = and (shiftRight number 16) 0xff, 
    g = and (shiftRight number 8) 0xff,
    b = and number 0xff
  }
