{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Trade where

import Control.Applicative
import Control.Arrow
import Data.Function (on)
import Data.List
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Generics

import Control.Lens
import qualified Data.Aeson as Aeson

data Dir
  = Buy
  | Sell
  deriving (Generic, Show)

data ByDir a =
  ByDir
    { buy :: a
    , sell :: a
    } deriving (Functor, Generic, Show)

byDir :: (Functor f) => Dir -> (a -> f a) -> ByDir a -> f (ByDir a)
byDir Buy  afa by = fmap (\a -> by{ buy  = a }) (afa (buy  by))
byDir Sell afa by = fmap (\a -> by{ sell = a }) (afa (sell by))

createByDir :: (Dir -> a) -> ByDir a
createByDir f = ByDir { buy = f Buy, sell = f Sell }

instance Applicative ByDir where
  pure = createByDir . const
  byf <*> byx = createByDir $ \dir -> (byf ^. byDir dir) (byx ^. byDir dir)

instance (Aeson.ToJSON a) => Aeson.ToJSON (ByDir a)
instance (Aeson.FromJSON a) => Aeson.FromJSON (ByDir a)

type Price = Rational
type Qty = Rational

data Order px qty =
  Order
    { orderPrice :: px
    , orderSize :: qty
    } deriving (Generic, Show)

instance (Aeson.ToJSON a, Aeson.ToJSON b) => Aeson.ToJSON (Order a b)
instance (Aeson.FromJSON a, Aeson.FromJSON b) => Aeson.FromJSON (Order a b)

type PriceLevel u = (Price, NonEmpty (u, Qty))

compareAgg :: Dir -> Price -> Price -> Ordering
compareAgg Buy = compare
compareAgg Sell = flip compare

aggressionLevelsForDir :: Dir -> [(u, Order Price Qty)] -> [PriceLevel u]
aggressionLevelsForDir dir orders =
  sortBy (flip (compareAgg dir) `on` (orderPrice . snd)) orders
  & foldr groupSameLevel []
  where
    groupSameLevel (u, Order{ orderPrice, orderSize }) priceLevels =
      let
        this = (u, orderSize)
      in
      case priceLevels of
        (px, people) : rest
          | px == orderPrice -> (px, NonEmpty.cons this people) : rest
        _ -> (orderPrice, this :| []) : priceLevels

aggressionLevels :: ByDir [(u, Order Price Qty)] -> ByDir [PriceLevel u]
aggressionLevels orders =
  ByDir
    { buy = aggressionLevelsForDir Buy (buy orders)
    , sell = aggressionLevelsForDir Sell (sell orders)
    }

levelSize :: NonEmpty (u, Rational) -> Rational
levelSize = sum . fmap snd

splitMatchingSizeLevels
  :: ByDir [PriceLevel u]
  -> ( [ByDir (PriceLevel u)]
     , Maybe (Dir, (NonEmpty (PriceLevel u)))
     )
splitMatchingSizeLevels ByDir{ buy = [], sell = [] } = ([], Nothing)
splitMatchingSizeLevels ByDir{ buy = (b : bs), sell = [] } = ([], Just (Buy, (b :| bs)))
splitMatchingSizeLevels ByDir{ buy = [], sell = (s : ss) } = ([], Just (Sell, (s :| ss)))
splitMatchingSizeLevels ByDir{ buy = ((bpx, bszs) : bs), sell = ((spx, sszs) : ss) } =
  case compare bSize sSize of
    EQ ->
      first
        (ByDir { buy = (bpx, bszs), sell = (spx, sszs) } :)
        (splitMatchingSizeLevels ByDir{ buy = bs, sell = ss })
    LT ->
      first
        (ByDir { buy = (bpx, bszs), sell = (spx, scale (bSize / sSize) sszs) } :)
        (splitMatchingSizeLevels
          ByDir
            { buy = bs
            , sell = (spx, scale (1 - bSize / sSize) sszs) : ss
            })
    GT ->
      first
        (ByDir { buy = (bpx, scale (sSize / bSize) bszs), sell = (spx, sszs) } :)
        (splitMatchingSizeLevels
          ByDir
            { buy = (bpx, scale (1 - sSize / bSize) bszs) : bs
            , sell = ss
            })
  where
    scale factor = fmap (\(u, q) -> (u, q * factor))
    bSize = levelSize bszs
    sSize = levelSize sszs

data AuctionResult u =
  AuctionResult
    { price :: Price
    , whoTraded :: ByDir (NonEmpty (u, Rational))
    } deriving (Generic, Show)

addMatchingSizeMostAggLevel
  :: ByDir (PriceLevel u)
  -> Maybe (ByDir (PriceLevel u))
  -> Maybe (ByDir (PriceLevel u))
addMatchingSizeMostAggLevel this@ByDir{ buy = (bpx, _), sell = (spx, _) } auctionInProgress
  | bpx < spx = Nothing
  | otherwise =
    Just $ maybe this (liftA2 addLevels this) auctionInProgress
    where
      addLevels (_, traded) (px, alreadyTraded) = (px, traded <> alreadyTraded)

finishAuction :: Maybe Dir -> ByDir (PriceLevel u) -> AuctionResult u
finishAuction extraDemand auctionInProgress =
  AuctionResult { price, whoTraded = fmap snd auctionInProgress }
  where
    ByDir{ buy = buyPrice, sell = sellPrice } = fmap fst auctionInProgress
    price =
      case extraDemand of
        Nothing -> (buyPrice + sellPrice) / 2
        Just Buy -> buyPrice
        Just Sell -> sellPrice

ofMatchingSizeAggLevels
  :: ( [ByDir (PriceLevel u)]
     , Maybe (Dir, (NonEmpty (PriceLevel u)))
     )
  -> Maybe (AuctionResult u)
ofMatchingSizeAggLevels (msAggLevels, extraDemand)
  =
  fmap (finishAuction (fmap fst extraDemand))
    (foldr addMatchingSizeMostAggLevel Nothing msAggLevels)

runAuction
  :: ByDir [(u, Order Price Qty)]
  -> Maybe (AuctionResult u)
runAuction orders =
  ofMatchingSizeAggLevels
    (splitMatchingSizeLevels (aggressionLevels orders))
