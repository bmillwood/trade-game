module ByResource exposing (..)

import Resource exposing (Resource)

type alias ByResource a = { mined : a, smelted : a }

get : Resource -> ByResource a -> a
get r =
  case r of
    Resource.Mined -> .mined
    Resource.Smelted -> .smelted

set : Resource -> a -> ByResource a -> ByResource a
set r x b =
  case r of
    Resource.Mined -> { b | mined = x }
    Resource.Smelted -> { b | smelted = x }

map : (a -> b) -> ByResource a -> ByResource b
map f b = { mined = f b.mined, smelted = f b.smelted }

both : a -> ByResource a
both x = { mined = x, smelted = x }
