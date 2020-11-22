module View.Style exposing (..)

import Css
import Html.Styled
import Html.Styled.Attributes

type alias Tag msg
  =  List (Html.Styled.Attribute msg)
  -> List (Html.Styled.Html msg)
  -> Html.Styled.Html msg

thead : Tag msg
thead =
  Html.Styled.styled
    Html.Styled.thead
    [ Css.fontWeight Css.bold
    , Css.backgroundColor (Css.hex "ccc")
    ]

tbody : Tag msg
tbody =
  Html.Styled.styled
    Html.Styled.tbody
    [ Css.backgroundColor (Css.hex "eee")
    ]

td : Tag msg
td =
  Html.Styled.styled
    Html.Styled.td
    [ Css.padding2 (Css.em 0.1) (Css.em 0.5)
    ]

number : Tag msg -> Tag msg
number tag =
  Html.Styled.styled
    tag
    [ Css.textAlign Css.right
    ]

tradeQty : Tag msg -> Tag msg
tradeQty tag =
  Html.Styled.styled
    (number tag)
    [ Css.width (Css.em 2)
    ]

errorDisplay : Tag msg
errorDisplay =
  Html.Styled.styled
    Html.Styled.pre
    [ Css.backgroundColor (Css.hsl 0 0.5 0.9)
    , Css.padding (Css.em 1)
    , Css.border3 (Css.px 1) Css.solid (Css.hsl 0 1 0.5)
    ]
