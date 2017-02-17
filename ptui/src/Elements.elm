module Elements exposing (..)

-- A module of higher level abstractions for UI elements.

import Html exposing (..)
import Html.Attributes exposing (..)
import Css as S

hbox : List (Html a) -> Html a
hbox els = habox [] els

habox : List (Attribute a) -> List (Html a) -> Html a
habox attrs els = div (attrs ++ [style [("display", "flex")]]) els

vbox : List (Html a) -> Html a
vbox els = vabox [] els

vabox : List (Attribute a) -> List (Html a) -> Html a
vabox attrs els = div (attrs ++ [style [("display", "flex"), ("flex-direction", "column")]]) els

datext a t = div a [text t]

dtext t = div [] [text t]


plainBorder = S.border3 (S.px 1) S.solid (S.rgb 0 0 0)
