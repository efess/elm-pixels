import Html exposing (Html,Attribute, div, h1, section, text, button, select, option)
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import List exposing (..)
import Array exposing (..)
import Pixel exposing (Pixel)
import PixelMatrix exposing (..)
import Rgb
import Animations.Animation exposing (AniState, AniResult, runFrame, setup)
import Time exposing (Time, millisecond)
import Window
import Task
import Json.Encode exposing (string)
import String

initialAnimation = "matrix" 

-- scaleTime: Float -> Int
-- scaleTime timeMilli= 
--   round (timeMilli / 10)

main = Html.program {
    init = init, 
    view = view,
    update = update,
    subscriptions = subscriptions
  }

-- MODEL

type alias Model = {
  windowSize: Window.Size,
  pixelMatrix : PixelMatrix,
  animationState: AniState,
  lastFrameNum: Int
}

init : (Model, Cmd Msg)
init =
  let
    initialWindowSize = Window.Size 0 0
  in
    (
      { 
        windowSize = initialWindowSize,
        pixelMatrix = PixelMatrix.empty 0 0,
        animationState = (setup initialAnimation initialWindowSize),
        lastFrameNum = 0
      },
      Task.perform Resize Window.size
    )

-- UPDATE

type Msg = Tick Time | AnimationChange String | Resize Window.Size

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Tick newTime ->
      let
        frameNum = round (Time.inMilliseconds newTime)
        aniResult = if frameNum /= model.lastFrameNum -- Why is tick firing twice for the same millisecond?
                    then runFrame newTime frameNum model.windowSize model.animationState
                    else {pixelMatrix = model.pixelMatrix, state =model.animationState}
      in
        (
          { 
            model |
              pixelMatrix = aniResult.pixelMatrix,
              animationState = aniResult.state,
              lastFrameNum = frameNum
          }, 
          Cmd.none
        )
    AnimationChange newAnimation -> 
      ( 
        { model | animationState = setup newAnimation model.windowSize },
        Cmd.none
      )
    Resize newSize ->
      (
        { model | windowSize = newSize }, 
        Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [
      Time.every (100 * millisecond) Tick,
      Window.resizes Resize
    ]

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    select [ placeholder "", onInput AnimationChange ] [
      option [ value "rainbow" ] [text("Spectrum")],
      option [ value "blips"] [text("Random")],
      option [ value "matrix", selected True] [text("Matrix")]
    ],
    div [] (List.map (div [class ("pixel-row")]) (renderPixelsDisplay model))
  ]
  

renderPixelsDisplay: Model -> List (List (Html Msg))
renderPixelsDisplay model = 
  Array.map (Array.toList << Array.map renderPixel) model.pixelMatrix
    |> Array.toList

renderPixel : Pixel -> Html msg
renderPixel pixel = 
  div [ 
    class ("pixel pixel" ++ toString pixel.id), 
    style [ 
      ("backgroundColor", "#" ++ Rgb.toHex pixel.backColor),
      ("color", "#" ++ Rgb.toHex pixel.foreColor),
      ("font-family", "matrix"),
      ("height", toString pixel.width ++ "px"),
      ("width", toString pixel.width ++ "px") ],
    --property "innerHTML" (string "&nbsp;")
    property "innerHTML" (string (String.fromChar pixel.glyph))
  ] [] -- TODO: Implement GLYPH
