{-# Language DataKinds     #-}
{-# Language TypeFamilies  #-}
{-# Language PolyKinds     #-}
{-# Language TypeOperators #-}
module TypeListOps where

import Data.Kind

type family All (cs :: [k -> Constraint]) (a :: k) :: Constraint where
  All '[]       a = ()
  All (c ': cs) a = (c a, All cs a)

type family Use (cs :: [k -> Constraint]) (a :: [k]) :: Constraint where
  Use cs '[]       = ()
  Use cs (a ': as) = (All cs a, Use cs as)
