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

pixelCount : number
pixelCount = 255

initialAnimation = "blips" 

scaleTime: Float -> Int
scaleTime timeMilli= 
  round (timeMilli / 10)

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
  animationState: AniState
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
        animationState = (setup initialAnimation initialWindowSize)
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
        frameNum = scaleTime (Time.inMilliseconds newTime)
        aniResult = runFrame newTime frameNum model.windowSize model.animationState
      in
        (
          { 
            model |
              pixelMatrix = aniResult.pixelMatrix,
              animationState = aniResult.state
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
      Time.every millisecond Tick,
      Window.resizes Resize
    ]

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    select [ placeholder "", onInput AnimationChange ] [
      option [ value "rainbow" ] [text("Spectrum")],
      option [ value "blips", selected True] [text("Random")],
      option [ value "matrix"] [text("Matrix")]
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
      ("height", toString pixel.width ++ "px"),
      ("width", toString pixel.width ++ "px") ],
    property "innerHTML" (string "&nbsp;")
  ] [] -- TODO: Implement GLYPH
