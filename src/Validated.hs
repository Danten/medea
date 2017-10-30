module Validated where

-- import           Data.Bifoldable
import           Data.Bifunctor
-- import           Data.Bitraversable
import           Data.List.NonEmpty
import           Data.Semigroup

data Validated e a
  = Invalid (NonEmpty e)
  | Valid a
  deriving Show

invalid :: e -> Validated e a
invalid e = Invalid (e :| [])

instance Functor (Validated e) where
  fmap = bimap id

instance Foldable (Validated e) where
  foldMap _ (Invalid _) = mempty
  foldMap f (Valid x)   = f x

instance Traversable (Validated e) where
  traverse _ (Invalid err) = pure (Invalid err)
  traverse f (Valid x)     = Valid `fmap` f x

instance Bifunctor Validated where
  bimap _ f (Valid x)     = Valid (f x)
  bimap e _ (Invalid err) = Invalid (fmap e err)

-- instance Bifoldable Validated where
--   bifoldMap f _ (Invalid err) = foldMap f err
--   bifoldMap _ f (Valid x) = f x


instance Applicative (Validated e) where
  pure = Valid
  Valid f <*> Valid x = Valid (f x)
  Valid _ <*> Invalid err = Invalid err
  Invalid err <*> Valid _ = Invalid err
  Invalid e0 <*> Invalid e1 = Invalid (e0 <> e1)

andThen :: Validated e a -> (a -> Validated e b) -> Validated e b
Valid x `andThen` f = f x
Invalid err `andThen` _ = Invalid err

fromMaybe :: e -> Maybe a -> Validated e a
fromMaybe e Nothing  = invalid e
fromMaybe _ (Just x) = pure x

toMaybe :: Validated e a -> Maybe a
toMaybe (Invalid _) = Nothing
toMaybe (Valid x) = Just x

fromBool :: e -> Bool -> Validated e ()
fromBool _ True = pure ()
fromBool e False = invalid e
