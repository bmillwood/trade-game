module ByDir exposing (..)

import Trade exposing (Dir)

type alias ByDir a = { buy : a, sell : a }

get : Dir -> ByDir a -> a
get d =
  case d of
    Trade.Buy -> .buy
    Trade.Sell -> .sell

create : (Dir -> a) -> ByDir a
create f =
  { buy = f Trade.Buy
  , sell = f Trade.Sell
  }

set : Dir -> a -> ByDir a -> ByDir a
set d x b =
  case d of
    Trade.Buy -> { b | buy = x }
    Trade.Sell -> { b | sell = x }

map : (a -> b) -> ByDir a -> ByDir b
map f b = create (\d -> f (get d b))

both : a -> ByDir a
both x = create (\_ -> x)
