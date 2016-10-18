import Html exposing (Html,Attribute, div, h1, section, text, button, select, option)
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

initialAnimation = "rainbow" 

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
  animationState: AniState
}

init : (Model, Cmd Msg)
init =
  ({ 
    pixels = [], 
    animationState = setup initialAnimation
  },
  Cmd.none)

-- UPDATE

type Msg = Tick Time | AnimationChange String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Tick newTime ->
      let
        frameNum = scaleTime (Time.inMilliseconds newTime)
        aniResult = runFrame newTime frameNum pixelCount model.animationState
      in
        ({
          pixels = aniResult.pixels,
          animationState = aniResult.state
        }, Cmd.none)
    AnimationChange newAnimation-> 
      ({ 
        pixels = [], 
        animationState = setup newAnimation
      },
      Cmd.none) 

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every millisecond Tick

-- VIEW

view : Model -> Html Msg
view model =
  div [] [
    select [ placeholder "", onInput AnimationChange ] [
      option [ value "rainbow" ] [text("Spectrum")],
      option [ value "blips" ] [text("Random")]
    ],
    div [] (renderPixelsDisplay model)
  ]
  

renderPixelsDisplay: Model -> List (Html Msg)
renderPixelsDisplay model = 
  map renderPixel model.pixels

renderPixel : Pixel -> Html msg
renderPixel pixel = 
  div [ 
    class ("pixel pixel" ++ toString pixel.id), 
    style [ ("backgroundColor", "#" ++ Rgb.toHex pixel.color) ]
  ] [text ("")]
