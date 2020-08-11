module ByResource exposing (..)

import Resource exposing (Resource)

type alias ByResource a = { mined : a, crafted : a }

get : Resource -> ByResource a -> a
get r =
  case r of
    Resource.Mined -> .mined
    Resource.Crafted -> .crafted

map : (a -> b) -> ByResource a -> ByResource b
map f b = { mined = f b.mined, crafted = f b.crafted }

both : a -> ByResource a
both x = { mined = x, crafted = x }
