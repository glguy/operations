{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language PolyKinds #-}
{-# Language TypeOperators #-}
module TypeListOps where

import Data.Kind

import NatOps

type family Index (x :: k) (xs :: [k]) :: Nat where
  Index x (x ': xs) = Z
  Index x (y ': xs) = S (Index x xs)

type family Image (xs :: [k]) (ys :: [k]) :: [Nat] where
  Image '[]       ys = '[]
  Image (x ': xs) ys = Index x ys ': Image xs ys

type family All (cs :: [k -> Constraint]) (a :: k) :: Constraint where
  All '[]       a = ()
  All (c ': cs) a = (c a, All cs a)

type family Use (cs :: [k -> Constraint]) (a :: [k]) :: Constraint where
  Use cs '[]       = ()
  Use cs (a ': as) = (All cs a, Use cs as)
