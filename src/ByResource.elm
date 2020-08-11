module ByResource exposing (..)

import Resource exposing (Resource)

type alias ByResource a = { mined : a, crafted : a }

get : Resource -> ByResource a -> a
get r =
  case r of
    Resource.Mined -> .mined
    Resource.Crafted -> .crafted

set : Resource -> a -> ByResource a -> ByResource a
set r x b =
  case r of
    Resource.Mined -> { b | mined = x }
    Resource.Crafted -> { b | crafted = x }

map : (a -> b) -> ByResource a -> ByResource b
map f b = { mined = f b.mined, crafted = f b.crafted }

both : a -> ByResource a
both x = { mined = x, crafted = x }
