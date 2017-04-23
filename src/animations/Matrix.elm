module Animations.Matrix exposing (getFrame, setup, MatrixState)
import Random exposing (Seed, int, step)
import Maybe
import Time exposing (Time)
import Pixel exposing (Pixel)
import PixelMatrix exposing (PixelMatrix, empty)
import Rgb exposing (..)
import Array exposing (..)
import Window
import Matrix exposing (..)
import Char exposing (..)
import Bitwise exposing (shiftRightBy)
import Animations.PixelDimensions exposing (PixelDimensions, getDimensions)
import MatrixFont exposing (intToChar, fontIntMin, fontIntMax)

-- Matrix as in the movie "the Matrix" computer screen
-- state: 
--  individual pixel state ???
--    stage =  0 - 255 level of "brightness" for pixel - 0 is "off", activated pixels start at 255
--      and decrement when "fading" towards tail
--    glyph to display when ON 
--    changeOfChange will determine if the character will change
--    changeOfTerminatingChange will determine if the character will *not* change anymore
--    flashAmount will determine if the character is in "flash" state and how much. 
--      Starts at 255, and decrements to zero - normal state.  Will blend in with "green amount"
--    color pulsating

--  trailState
--    length
--    startLocation 
--    currentLocation
--    speed
--    flashType = | flashy | noFlash

trailSpawnChance = 3 -- 1 in this number frames
pixelSize = 20

pixelStepGenerator = Random.int 15 10000
pixelColorGenerator = Random.int 0 5

type FlashType = Flashy | NotFlashy

type alias PixelParams = {
  stage: Int,
  flashAmount: Int,
  greenAmount: Int,
  chanceOfGlyphChange: Int,
  chanceOfTerminatingChange: Int,
  glyph: Char
}

type alias Trail = {
  length: Int,
  headCol: Int,
  headRow: Int,
  speed: Int,  -- 1 to infinity -> the larger the number the slower
  flashType: FlashType
}

type alias MatrixState = {
  pixelParams: Matrix PixelParams,
  trails: List Trail,
  dimensions: PixelDimensions
}

emptyParams: PixelParams
emptyParams = {
    stage = 0, 
    flashAmount = 0, 
    greenAmount = 0, 
    chanceOfGlyphChange = 0, 
    chanceOfTerminatingChange = 0, 
    glyph = ' '
  }

emptyTrail: Trail
emptyTrail = { length = 0, headCol = 0, headRow = 0, speed= 0, flashType = NotFlashy}

emptyParamsMatrix: Int -> Int -> Matrix PixelParams
emptyParamsMatrix width height = Matrix.matrix height width (\_ -> emptyParams)

emptyMatrixState: MatrixState
emptyMatrixState = {
    pixelParams = emptyParamsMatrix 0 0,
    trails = [],
    dimensions = { height = 0, total = 0, width = 0}
  }

trailExists: Int -> List Trail -> Bool
trailExists col = List.any (\t -> t.headCol == col)

-- trailFindAtPixel: Int -> Int -> List Trail -> Trail
-- trailFindAtPixel col row = List.find (\t -> t.headCol == col && t.headRow == row)

addNewTrail: List Trail -> Seed -> Int -> (List Trail, Seed)
addNewTrail trails seed col =
  let
    -- TODO: Randomize props
    randomLength = Random.step (Random.int 50 60) seed
    newList =  {
      emptyTrail |  headCol = col,
                    speed = 10, 
                    length = Tuple.first randomLength
    } :: trails
  in
    (newList, Tuple.second randomLength)

randomTrail: Int -> List Trail -> Seed -> (List Trail, Seed)
randomTrail maxTrails trails seed =
  let 
    randomTrailRes = Random.step (Random.int 0 maxTrails) seed
    trailCol = Tuple.first randomTrailRes
    newSeed = Tuple.second randomTrailRes
  in
    if trailExists trailCol trails
    then randomTrail maxTrails trails newSeed 
    else addNewTrail trails newSeed trailCol
    
randomTrails: Int -> List Trail -> Seed -> (List Trail, Seed)
randomTrails maxTrails trails seed = 
  let
    spawnChance = if List.length trails < maxTrails 
                  then Random.step (Random.int 0 30) seed 
                  else (0, seed)
  in
    if Tuple.first spawnChance > 5
    then randomTrail maxTrails trails (Tuple.second spawnChance)
    else (trails, seed)

stepTrail: Trail -> Trail
stepTrail trail = {
    trail | headRow = trail.headRow + 1
  }

stepTrails: List Trail -> Int -> List Trail
stepTrails trails frameNum = List.map (\t -> if frameNum % t.speed == 0 then stepTrail t else stepTrail t) trails

purgeTrails: Int -> List Trail -> List Trail
purgeTrails maxRow =
  List.filter (\t -> t.headRow < maxRow)

isTrailStart: List Trail -> Location -> Bool
isTrailStart trails pixelPos = 
  List.any (\t -> t.headCol == col pixelPos && t.headRow == row pixelPos) trails

applyTrail: (Matrix PixelParams, Seed) -> Trail -> (Matrix PixelParams, Seed)
applyTrail pixelParamsAndSeed trail  =
  let
    pixelParam = newPixelParamOnTrail trail (Tuple.second pixelParamsAndSeed)
    pixelParams = Tuple.first pixelParamsAndSeed
    location = Matrix.loc trail.headRow trail.headCol
  in
    ( Matrix.set location (Tuple.first pixelParam) pixelParams, Tuple.second pixelParam)

applyTrails: List Trail -> (Matrix PixelParams, Seed) -> (Matrix PixelParams, Seed)
applyTrails trails pixelParamsAndSeed = 
  let
    head = List.head trails
    tail = Maybe.withDefault [] (List.tail trails)
  in
    if head /= Nothing
    then applyTrails tail (applyTrail pixelParamsAndSeed (Maybe.withDefault emptyTrail head))
    else pixelParamsAndSeed


  -- List.head trails
  --  |> Maybe.andThen (applyTrail pixelParamsAndSeed)
  --  |> applyTrails (Maybe.withDefault [] (List.tail trails))
    
newPixelParamOnTrail: Trail -> Seed -> (PixelParams, Seed)
newPixelParamOnTrail trail seed = 
  let
    glyphChange = Random.step (Random.int 0 20) seed
    termChange = Random.step (Random.int 10 200) (Tuple.second glyphChange)
    glyph = randomGlyph (Tuple.second termChange)
  in
    ({
      stage = trail.length,
      flashAmount = 255,
      greenAmount = 200,
      chanceOfGlyphChange = Tuple.first glyphChange,
      chanceOfTerminatingChange = Tuple.first termChange,
      glyph = Tuple.first glyph
    }, Tuple.second glyph)

randomGlyph: Seed -> (Char, Seed)
randomGlyph seed =
  let
    nextGlyph = Random.step (Random.int 26 MatrixFont.fontIntMax) seed
  in
    (MatrixFont.intToChar (Tuple.first nextGlyph), Tuple.second nextGlyph)

addRandomGlyph: PixelParams -> Seed -> (PixelParams, Seed)
addRandomGlyph pixelParams seed =
  let
    charSeed = randomGlyph seed
    randomGreen = Random.step (Random.int 60 255) (Tuple.second charSeed)
  in
    ({pixelParams | glyph = Tuple.first charSeed, greenAmount = Tuple.first randomGreen}, Tuple.second randomGreen)


updatePixelParam: Int -> Location -> PixelParams -> Seed -> (PixelParams, Seed)
updatePixelParam frameNum location p seed =
  let
    stopGlyphChange = if p.chanceOfTerminatingChange > 0
                      then Random.step (Random.int 1 p.chanceOfTerminatingChange) seed
                      else (0, seed)
    
    stopGlyphChangeVal = Tuple.first stopGlyphChange

    needsGlyphChange = if p.chanceOfTerminatingChange > 0
                       then Random.step (Random.int 0 p.chanceOfGlyphChange) (Tuple.second stopGlyphChange)
                       else (0, Tuple.second stopGlyphChange)
    
    needsGlyphChangeVal = Tuple.first needsGlyphChange

    nextseed = Tuple.second needsGlyphChange
    updatedPixelParam = { p | stage = p.stage - 1,
                              chanceOfTerminatingChange = if stopGlyphChangeVal /= p.chanceOfTerminatingChange 
                                                          then  p.chanceOfTerminatingChange else 0,
                              --greenAmount = updateGreenAmount p.stage p.greenAmount,
                              flashAmount = if p.flashAmount > 30 then p.flashAmount - 30 else 0
                        }
  in
    if (needsGlyphChangeVal) == p.chanceOfGlyphChange && needsGlyphChangeVal > 0
    then addRandomGlyph updatedPixelParam nextseed
    else (updatedPixelParam, nextseed)

createAndUpdatePixelParam: Int -> List Trail -> Seed -> Location -> PixelParams -> (PixelParams, Seed)
createAndUpdatePixelParam frameNum trails seed location pixelParam =
    if pixelParam.stage > 0 && not (isTrailStart trails location) 
    then updatePixelParam frameNum location pixelParam seed
    else (pixelParam, seed)

updateMatrixPixelParam: Int -> Location -> (MatrixState, Seed) -> (MatrixState, Seed)
updateMatrixPixelParam frameNum location stateAndSeed =
  let
    state = Tuple.first stateAndSeed
    seed = Tuple.second stateAndSeed
    pixelParam = Maybe.withDefault emptyParams (Matrix.get location state.pixelParams)
    updatedPixelParam = createAndUpdatePixelParam frameNum state.trails seed location pixelParam
  in
    (
      {state | pixelParams = Matrix.set location (Tuple.first updatedPixelParam) state.pixelParams}, 
      Tuple.second updatedPixelParam
    )

updateMatrixPixelParams: Int -> Int -> (MatrixState, Seed) -> (MatrixState, Seed)
updateMatrixPixelParams frameNum index stateAndSeed =
  let
    state = Tuple.first stateAndSeed
    x = index % state.dimensions.width
    y = floor (toFloat index / toFloat state.dimensions.width)
    location = Matrix.loc y x
  in
    if index == state.dimensions.total then stateAndSeed
    else updateMatrixPixelParam frameNum location stateAndSeed
      |> updateMatrixPixelParams frameNum (index + 1)


updateState: MatrixState -> Time -> Int -> MatrixState
updateState state time frameNum =

  let
    initalSeed = Time.inMilliseconds time
      |> round
      |> Random.initialSeed
  
    trailsUpdate = randomTrails state.dimensions.width state.trails initalSeed
    trailsStepped = stepTrails (Tuple.first trailsUpdate) frameNum
    trailsPurged = purgeTrails state.dimensions.height trailsStepped 
    trailsApplied = applyTrails state.trails (state.pixelParams,  Tuple.second trailsUpdate)

    pixelsUpdate = ({ state | trails = trailsPurged, pixelParams = Tuple.first trailsApplied}, Tuple.second trailsApplied)
      |> updateMatrixPixelParams frameNum 0 
  in
    Tuple.first pixelsUpdate

-- randomPixelParam: Seed -> {pixelParam: PixelParams, seed: Seed}
-- randomPixelParam seed =
--   let
--     rndColor = Random.step pixelColorGenerator seed
--     rndOffset = Tuple.second rndColor 
--       |> Random.step pixelStepGenerator 
--   in
--     {
--       pixelParam = {
--         color = Tuple.first rndColor
--           |> numberToColor,
--         offset = Tuple.first rndOffset
--       },
--       seed = Tuple.second rndOffset
--     }



-- updateState: Time -> MatrixState -> PixelDimensions -> MatrixState
-- updateState time currentState newDimensions =
--   let
--     initalSeed = Time.inMilliseconds time
--       |> round
--       |> Random.initialSeed
--     expandedMatrix = expandParamsMatrix newDimensions.width newDimensions.height currentState.pixelParams
--     pixelParams = fillExpandedPixelParmas currentState.dimensions 0 newDimensions.total expandedMatrix initalSeed
--   in
--     {
--       pixelParams = pixelParams.result,
--       dimensions = newDimensions
--     }

-- getNormalPixelColor: Color -> Int ->  Color
-- getNormalPixelColor color frameNum =
--   let 
--     greenBlue = { r = 0, g = 100, b = 100}
--     alterAmount = round(150 * weirdPeriodic (frameNum + 0))
--   in
--     {
--       r = greenBlue.r,--  + alterAmount,
--       g = greenBlue.g + alterAmount,--color.g,
--       b = greenBlue.b--  + alterAmount
--     }

-- createPixel: MatrixState -> Int -> Location -> PixelParams -> Pixel
-- createPixel state frameNum location pixelParams =
--   let 
--     color = getNormalPixelColor pixelParams.color frameNum
--   in
--     {
--       backColor = color, 
--       foreColor = color,
--       glyph = 'k',
--       id = (row location * state.dimensions.width) + col location,
--       x = 0,
--       y = 0,
--       width = pixelSize
--     }

-- uint32_t blendA(uint32_t color1, uint32_t color2, uint8_t alph) {
--     uint16_t alpha = alph + 1;
--     uint16_t inv_alpha = 256 - alph;

--     return combine((uint16_t)(red(color1) * alpha + inv_alpha * (uint16_t)red(color2)) >> 8,
--         ((uint16_t)green(color1) * alpha + inv_alpha * (uint16_t)green(color2)) >> 8,
--         ((uint16_t)blue(color1) * alpha + inv_alpha * (uint16_t)blue(color2)) >> 8);
-- }

blendA: Color -> Color -> Int -> Color
blendA colorOne colorTwo blendAmount =
  let
    alpha = blendAmount + 1
    invalpha = 256 - alpha
  in
    {
      r = shiftRightBy 8 ((colorOne.r * alpha) + (invalpha * colorTwo.r)),
      g = shiftRightBy 8 ((colorOne.g * alpha) + (invalpha * colorTwo.g)),
      b = shiftRightBy 8 ((colorOne.b * alpha) + (invalpha * colorTwo.b))
    }

expandParamsMatrix: Int -> Int -> Matrix PixelParams -> Matrix PixelParams
expandParamsMatrix width height matrix =
  Matrix.matrix height width (\l -> Maybe.withDefault emptyParams (Matrix.get l matrix))

pixelParamToPixel: Location -> PixelParams -> Pixel
pixelParamToPixel location pixelParam = 
  let
    green = {r = 0, g = pixelParam.greenAmount, b = 0} 

    greenWithFade = if pixelParam.stage < 20 
                    then blendA green {r = 0, g = 0, b = 0 } (round ((toFloat pixelParam.stage / toFloat 20) * 255.0))
                    else green

    greenWithFadeAndFlash = if pixelParam.flashAmount > 0
                then blendA {r = 255, g = 255, b = 255 } greenWithFade pixelParam.flashAmount
                else greenWithFade
  in
  if pixelParam.stage == 0 
  then 
    {
      id = col location * row location,
      x = col location ,
      y = row location,
      width = pixelSize,
      glyph = ' ',
      backColor = {r = 0, g = 0, b = 0},
      foreColor = greenWithFadeAndFlash
    }

  else
    {
      id = col location * row location,
      x = col location ,
      y = row location,
      width = pixelSize,
      glyph = pixelParam.glyph,
      backColor = {r = 0, g = 0, b = 0},
      foreColor = greenWithFadeAndFlash
    }


getFrame: Time -> Int -> Window.Size -> MatrixState -> (PixelMatrix, MatrixState)
getFrame time frameNum windowSize state =
  let 
    dimensions = getDimensions windowSize pixelSize
    sizedState = if dimensions.width /= state.dimensions.width || dimensions.height /= state.dimensions.height
                  then {state | pixelParams = expandParamsMatrix dimensions.width dimensions.height state.pixelParams, 
                                dimensions = dimensions}
                  else state

    updatedState =  updateState sizedState time frameNum
  in
    (
      Matrix.mapWithLocation pixelParamToPixel updatedState.pixelParams, 
      updatedState
    )

setup: Window.Size -> MatrixState
setup viewSize = 
  emptyMatrixState
