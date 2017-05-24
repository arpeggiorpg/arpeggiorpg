module Elements exposing (..)

-- A module of higher level abstractions for UI elements.

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css as S
import Json.Decode as JD

stdStyle = []

hbox : List (Html a) -> Html a
hbox els = habox stdStyle els

habox : List (Attribute a) -> List (Html a) -> Html a
habox attrs els = sdiv (attrs ++ [style [("display", "flex")]]) els

vbox : List (Html a) -> Html a
vbox els = vabox stdStyle els

vabox : List (Attribute a) -> List (Html a) -> Html a
vabox attrs els = sdiv (attrs ++ [style [("display", "flex"), ("flex-direction", "column")]]) els

datext a t = sdiv a [text t]

dtext t = sdiv [] [text t]

abspos left_ top_ = [S.position S.fixed, S.left left_, S.top top_]
overlay left_ top_ extra =
  sdiv <| stdStyle ++ [s <| (abspos left_ top_) ++ extra ++ [plainBorder, S.backgroundColor (S.rgb 255 255 255)]]

overlayRight right_ top_ extra = 
  sdiv <| stdStyle ++ [s <| [S.position S.fixed, S.right right_, S.top top_] ++ extra ++ [plainBorder, S.backgroundColor (S.rgb 255 255 255)]]

plainBorder = S.border3 (S.px 1) S.solid (S.rgb 0 0 0)

button : List (Attribute msg) -> List (Html msg) -> Html msg
button attrs contents =
  Html.button (stdStyle ++ [s [S.margin (S.px 0)]] ++ attrs) contents

sqButton : Float -> List (Attribute msg) -> List (Html msg) -> Html msg
sqButton size attrs content =
  button ([s [S.height (S.px size), S.width (S.px size)]] ++ attrs) content

s : List S.Mixin -> Attribute msg
s = S.asPairs >> Html.Attributes.style

hline = hr (stdStyle ++ [s [S.width (S.pct 100)]]) []

sdiv attrs body = div (attrs ++ stdStyle) body

clickable = s [S.cursor S.pointer]

noUserSelect =
  List.map (\name -> S.property name "none") ["-webkit-user-select", "-khtml-user-select", "-moz-user-select", "-ms-user-select", "user-select"]

icon attrs name = i (attrs ++ [class "material-icons", s noUserSelect]) [text name]

gear = icon [] "settings"
gearBox = icon [] "settings_applications"
threeDots = icon [] "more_horiz"

clickableIcon attrs name = icon (attrs ++ [clickable]) name

fullscreen : Html msg -> Html msg
fullscreen content =
  div [s [ S.position S.fixed
         , S.left (S.px 0), S.top (S.px 0)
         , S.height (S.pct 100), S.width (S.pct 100)
         , S.backgroundColor (S.rgb 255 255 255)]]
      [content]

scaleStyle : Float -> List (String, String)
scaleStyle scale = [ ("transform-origin", "top left")
                   , ("transform", "scale(" ++ toString scale ++ ")") ]

onEnterOrEsc : msg -> msg -> List (Attribute msg)
onEnterOrEsc submit cancel =
  let
    gotKey code =
      if code == 13 then JD.succeed submit
      else if code == 27 then JD.succeed cancel
      else JD.fail "not key we want"
  in
    [ on "keydown" (JD.andThen gotKey keyCode)
    , onBlur cancel
    , on "focusout" (JD.succeed cancel)]

textInput : List (Attribute msg) -> msg -> msg -> Html msg
textInput attrs submit cancel =
  input ([type_ "text"] ++ attrs ++ onEnterOrEsc submit cancel) []

