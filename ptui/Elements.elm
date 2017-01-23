module Elements exposing (..)

-- A module of higher level abstractions for UI elements.

import Html exposing (..)
import Html.Attributes exposing (..)

hbox : List (Html a) -> Html a
hbox els = div [style [("display", "flex"), ("width", "100%")] ]
               (List.map (\el -> div [style [("flex-grow", "1")]] [el]) els)

vbox : List (Html a) -> Html a
vbox els = div [style [("display", "flex"), ("flex-direction", "column"), ("width", "100%")]]
               (List.map (\el -> div [style [("flex-grow", "1")]] [el]) els)

-- An element whose *center* is positioned relative to its parents at specific x/y pixel offsets.
centerPositionedBox : String -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
centerPositionedBox x y attrs content =
  div [style [ ("position", "absolute")
             , ("left", x)
             , ("top", y)]]
      [div (attrs ++ [style [("position", "relative"), ("margin-left", "-50%"), ("margin-top", "-50%")]])
           content]
