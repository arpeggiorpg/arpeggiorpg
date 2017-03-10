module Elements exposing (..)

-- A module of higher level abstractions for UI elements.

import Html exposing (..)
import Html.Attributes exposing (..)
import Css as S

-- oh god why can't I just use a .css file
stdStyle = [s [S.boxSizing S.borderBox]]

hbox : List (Html a) -> Html a
hbox els = habox stdStyle els

habox : List (Attribute a) -> List (Html a) -> Html a
habox attrs els = div (attrs ++ stdStyle ++ [style [("display", "flex")]]) els

vbox : List (Html a) -> Html a
vbox els = vabox stdStyle els

vabox : List (Attribute a) -> List (Html a) -> Html a
vabox attrs els = div (attrs ++ stdStyle ++ [style [("display", "flex"), ("flex-direction", "column")]]) els

datext a t = div (stdStyle ++ a) [text t]

dtext t = div stdStyle [text t]

abspos left_ top_ = [S.position S.absolute, S.left left_, S.top top_]
overlay left_ top_ extra =
  div <| stdStyle ++ [s <| (abspos left_ top_) ++ extra ++ [plainBorder, S.backgroundColor (S.rgb 255 255 255)]]

overlayRight right_ top_ extra = 
  div <| stdStyle ++ [s <| [S.position S.absolute, S.right right_, S.top top_] ++ extra ++ [plainBorder, S.backgroundColor (S.rgb 255 255 255)]]

plainBorder = S.border3 (S.px 1) S.solid (S.rgb 0 0 0)

button : List (Attribute msg) -> List (Html msg) -> Html msg
button attrs contents =
  Html.button (stdStyle ++ [s [S.margin (S.px 0)]] ++ attrs) contents

sqButton : Float -> List (Attribute msg) -> List (Html msg) -> Html msg
sqButton size attrs content =
  button ([s [S.height (S.px size), S.width (S.px size)]] ++ attrs) content

s : List S.Mixin -> Attribute msg
s = S.asPairs >> Html.Attributes.style
