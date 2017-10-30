module Medea
  ( Decoder
  , Validated(..)
  , invalid
  , decode
  , attr
  , Decoder.opt
  , Decoder.validate
  , uniqueTag
  , uniqueText
  , isTag
  , Decoder.children
  , child
  ) where

import           Data.Functor.Identity
import           Data.Text     (Text)
import           Text.XML

import           Decoder       (Decoder)
import qualified Decoder
import           Validated

decode :: Document -> Decoder a -> Validated String a
decode doc dec = Decoder.runDecoder dec (documentRoot doc)

attr :: Name -> Decoder Text
attr name = Decoder.attr name $
  "Can't find attribute: " ++ show name

uniqueTag :: Name -> Decoder a -> Decoder a
uniqueTag name = fmap runIdentity . Decoder.nodes p
  where
    p :: [Node] -> Validated String (Identity Element)
    p ns = case [ e | NodeElement e <- ns, elementName e == name] of
      [e] -> pure $ pure e
      [] -> invalid $ "Could not find tag " ++ show name
      _ -> invalid $ "Tag " ++ show name ++ " is not unique"

uniqueText :: Decoder Text
uniqueText = Decoder.selectNodes p
  where
    p :: [Node] -> Validated String Text
    p ns = case [ t | NodeContent t <- ns] of
      [t] -> pure t
      _ -> invalid "Can't find text"

isTag :: Name -> Decoder a -> Decoder a
isTag name = Decoder.filter $ fromBool msg . (name ==) . elementName
  where msg = "Current node is not a " ++ show name

child :: Show a => Decoder a -> Decoder a
child dec = Decoder.children dec `Decoder.validate` p
  where
    p :: Show a => [a] -> Validated String a
    p [x] = pure x
    p [] = invalid "The child is not unique"
    p xs = invalid $ "Is not unique: " ++ show xs
