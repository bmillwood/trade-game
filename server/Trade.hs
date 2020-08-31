{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Trade where

import Control.Arrow
import Data.List
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Ord
import GHC.Generics

import Control.Lens
import qualified Data.Aeson as Aeson

data TradeParam a =
  TradeParam
    { giveMax :: a
    , getForEachGive :: a
    } deriving (Generic, Show)

instance (Aeson.ToJSON a) => Aeson.ToJSON (TradeParam a)
instance (Aeson.FromJSON a) => Aeson.FromJSON (TradeParam a)

type Qty = Rational
type PriceLevel u = (Rational, NonEmpty (u, Qty))

aggressionLevels :: [(u, TradeParam Rational)] -> [PriceLevel u]
aggressionLevels tps =
  sortBy (comparing (getForEachGive . snd)) tps
  & foldr groupSameLevel []
  where
    groupSameLevel (u, TradeParam{ getForEachGive, giveMax }) priceLevels =
      let
        this = (u, giveMax)
      in
      case priceLevels of
        (px, people) : rest
          | px == getForEachGive -> (px, NonEmpty.cons this people) : rest
        _ -> (getForEachGive, this :| []) : priceLevels

levelSize :: NonEmpty (u, Rational) -> Rational
levelSize = sum . fmap snd

splitMatchingSizeLevels
  :: [PriceLevel u]
  -> [PriceLevel u]
  -> ( [(PriceLevel u, PriceLevel u)]
     , Maybe (Either (NonEmpty (PriceLevel u)) (NonEmpty (PriceLevel u)))
     )
splitMatchingSizeLevels [] [] = ([], Nothing)
splitMatchingSizeLevels (l : ls) [] = ([], Just (Left (l :| ls)))
splitMatchingSizeLevels [] (r : rs) = ([], Just (Right (r :| rs)))
splitMatchingSizeLevels ((lpx, ls) : lss) ((rpx, rs) : rss) =
  case compare lSize rSize of
    EQ ->
      first
        (((lpx, ls), (rpx, rs)) :)
        (splitMatchingSizeLevels lss rss)
    LT ->
      first
        (((lpx, ls), (rpx, scale (lSize / rSize) rs)) :)
        (splitMatchingSizeLevels lss ((rpx, scale (1 - lSize / rSize) rs) : rss))
    GT ->
      first
        (((lpx, scale (rSize / lSize) ls), (rpx, rs)) :)
        (splitMatchingSizeLevels ((lpx, scale (1 - rSize / lSize) ls) : lss) rss)
  where
    scale factor = fmap (\(u, q) -> (u, q * factor))
    lSize = levelSize ls
    rSize = levelSize rs

data AuctionResult u =
  AuctionResult
    { worstLeftPrice :: Rational
    , worstRightPrice :: Rational
    , leftsTraded :: NonEmpty (u, Rational)
    , rightsTraded :: NonEmpty (u, Rational)
    } deriving (Generic, Show)

ofMatchingSizeAggLevels
  :: ( [(PriceLevel u, PriceLevel u)]
     , Maybe (Either (NonEmpty (PriceLevel u)) (NonEmpty (PriceLevel u)))
     )
  -> Maybe (AuctionResult u)
ofMatchingSizeAggLevels ([], _) = Nothing
ofMatchingSizeAggLevels (((lpx, ls), (rpx, rs)) : _vs, _extraDemand)
  | lpx * rpx > 1 = Nothing
  | otherwise =
    Just $ AuctionResult
      { worstLeftPrice = lpx
      , worstRightPrice = rpx
      , leftsTraded = ls
      , rightsTraded = rs
      }

runAuction
  :: [(u, TradeParam Rational)]
  -> [(u, TradeParam Rational)]
  -> Maybe (AuctionResult u)
runAuction lefts rights =
  ofMatchingSizeAggLevels
    (splitMatchingSizeLevels (aggressionLevels lefts) (aggressionLevels rights))
