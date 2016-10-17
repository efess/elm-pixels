import Html exposing (Html,Attribute, div, h1, section, text, button, input)
import Html.App as App
import Html.Events exposing (onInput)
import Html.Attributes exposing (..)
import List exposing (..)
import Pixel exposing (Pixel)
import String
import Rgb
import Animations.Animation exposing (AniState, AniResult, runFrame, setup)
import Time exposing (Time, millisecond)

pixelCount : number
pixelCount = 255

initialAnimation = "blips" 

scaleTime: Float -> Int
scaleTime timeMilli= 
  round (timeMilli / 10)

main = App.program {
    init = init, 
    view = view,
    update = update,
    subscriptions = subscriptions
  }

-- MODEL

type alias Model = {
  pixels : List Pixel,
  animationState: AniState,
  time: Time
}

init : (Model, Cmd Msg)
init =
  ({ 
    pixels = [], 
    time = 0, 
    animationState = setup initialAnimation
  },
  Cmd.none)

-- UPDATE

type Msg = 
  Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Tick newTime ->
      let
        frameNum = (scaleTime (Time.inMilliseconds newTime))
        aniResult = runFrame frameNum pixelCount model.animationState
      in
        ({
          time = newTime,
          pixels = aniResult.pixels,
          animationState = aniResult.state
        }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every millisecond Tick

-- VIEW

view : Model -> Html Msg
view model =
  div [] (renderPixelsDisplay model)

renderPixelsDisplay: Model -> List (Html Msg)
renderPixelsDisplay model = 
  map renderPixel model.pixels

renderPixel : Pixel -> Html msg
renderPixel pixel = 
  div [ 
    class ("pixel pixel" ++ toString pixel.id), 
    style [ ("backgroundColor", "#" ++ Rgb.toHex pixel.color) ]
  ] [text ("")]
