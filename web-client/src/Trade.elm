module Trade exposing (..)

type Dir = Buy | Sell

type alias Order px qty = { price : px, size : qty }
