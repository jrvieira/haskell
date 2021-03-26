module Data.Type where

import Data.Map.Lazy as Map
import Data.Set as Set

-- basic numeric type and necessary operations

newtype Some = Some { some :: Int }

instance Eq Some where
   Some a == Some b = a == b

instance Ord Some where
   compare (Some a) (Some b) = compare a b

instance Show Some where
   show (Some n) = show n

instance Semigroup Some where
   Some a <> Some b = Some (a + b)

instance Monoid Some where
   mempty = Some 0

-- collections

newtype Map' a = Map' { map' :: Map a Some }

get :: Ord a => a -> Map' a -> Some
get k m = findWithDefault mempty k (map' m)

instance Ord a => Semigroup (Map' a) where
   Map' a <> Map' b = Map' (unionWith (<>) a b)

instance Ord a => Monoid (Map' a) where
   mempty = Map' mempty

-- set

newtype Set' a = Set' { set' :: Set a }

has :: Ord a => a -> Set' a -> Bool
has x (Set' s) = Set.member x s

instance Ord a => Semigroup (Set' a) where
   Set' a <> Set' b = Set' (Set.union a b)

instance Ord a => Monoid (Set' a) where
   mempty = Set' Set.empty

--

--instance Semigroup X where
--   a <> b = X
--      (on (+) α a b)
--      (on (+) ω a b)
--      (on (+) φ a b)
--      (on (+) ε a b)
--      (on (+) ψ a b)
