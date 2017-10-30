module Decoder where

import qualified Data.Map  as Map
import           Data.Text (Text)
import           Text.XML

import           Validated (Validated (..), andThen, fromMaybe, toMaybe)

newtype Decoder' e a = Decoder
  { runDecoder :: Element -> Validated e a}

type Decoder = Decoder' String

instance Functor (Decoder' e) where
  fmap f (Decoder dec) = Decoder (fmap (fmap f) dec)

instance Applicative (Decoder' e) where
  pure = Decoder . pure . pure
  Decoder f <*> Decoder x = Decoder $ (<*>) <$> f <*> x

nodes :: Traversable f => ([Node] -> Validated e (f Element))
  -> Decoder' e a -> Decoder' e (f a)
nodes p d = Decoder $ \el ->
  p (elementNodes el) `andThen` traverse (runDecoder d)

selectNodes :: ([Node] -> Validated e a) -> Decoder' e a
selectNodes f = Decoder $ f . elementNodes

attr :: Name -> e -> Decoder' e Text
attr n e = Decoder $ fromMaybe e . Map.lookup n . elementAttributes

opt :: Decoder a -> Decoder (Maybe a)
opt d = Decoder $ pure . toMaybe . runDecoder d

validate :: Decoder' e a -> (a -> Validated e b) -> Decoder' e b
validate dec v = Decoder $ \el ->
  runDecoder dec el `andThen` v

children :: Decoder' e a -> Decoder' e [a]
children dec = Decoder $ \el ->
  Valid [x | NodeElement e <- elementNodes el
           , Valid x <- [runDecoder dec e]]

filter :: (Element -> Validated e ()) -> Decoder' e a -> Decoder' e a
filter p d = Decoder $ \el ->
  p el `andThen` const (runDecoder d el)
