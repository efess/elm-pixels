module PixelMatrix exposing (PixelMatrix, empty, adjustSize, listToMatrix)
import Matrix exposing (..)
import Pixel exposing (..)
import Array exposing (get, fromList)

type alias PixelMatrix = Matrix Pixel

blankPixel: Location -> Pixel
blankPixel loc =
  Pixel.emptyPixel

empty: Int -> Int -> PixelMatrix
empty m n = Matrix.matrix m n blankPixel

getMat: PixelMatrix -> Location -> Pixel
getMat mat loc =
  Maybe.withDefault Pixel.emptyPixel (Matrix.get loc mat)

getVec: List Pixel -> Int -> Pixel
getVec vec x =
  Maybe.withDefault Pixel.emptyPixel (Array.get x (Array.fromList vec))

adjustSize: PixelMatrix -> Int -> Int -> PixelMatrix
adjustSize oldMatrix newWidth newHeight = 
  Matrix.matrix newHeight newWidth (getMat oldMatrix)

addRow: PixelMatrix -> List Pixel -> PixelMatrix
addRow matrix vector =
  let
    newHeight = Matrix.colCount matrix
  in
    Matrix.matrix newHeight newHeight 
      (\l -> if Matrix.col l > newHeight then (getMat matrix l) else (getVec vector (Matrix.col l)))

plistToMatrix: List Pixel -> Int -> List (List Pixel) -> List (List Pixel)
plistToMatrix pixels cols partialLists =
  if List.length pixels == 0 || cols == 0 then partialLists
  else
    let
      pix = List.take cols pixels
    in
      plistToMatrix (List.drop cols pixels) cols (List.append partialLists [pix])

listToMatrix: List Pixel -> Int -> PixelMatrix
listToMatrix pixels cols =
  plistToMatrix pixels cols [[]]
    |> Matrix.fromList