{-# Language ExistentialQuantification #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeFamilies #-}
{-# Language RankNTypes #-}
{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language FlexibleInstances, FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}
{-# Language InstanceSigs #-}
{-# Language TypeApplications, AllowAmbiguousTypes #-}

module Object where

import Data.Kind
import Data.Proxy
import TypeListOps
import NatOps

data Object cs = forall a. All cs a => Object a

exampleUpcast :: Object '[Read,Show] -> Object '[Show]
exampleUpcast = upcast

exampleShow :: Show ∈ cs => Object cs -> String
exampleShow o = using1 @Show o show

exampleShow1 :: Object '[Read, Show] -> String
exampleShow1 = exampleShow

-- | List of objects that can be tracked with any set of constraints, as
-- long as those constraints are available for Int and Double.
producer1 :: Use cs '[Int, Double] => [Object cs]
producer1 = [Object (10 :: Int), Object (4.2 :: Double), Object (5.3 :: Double)]

producer2 :: Use cs '[Integer, Float] => [Object cs]
producer2 = [Object (100 :: Integer), Object (1.0 :: Float)]


example :: [String]
example = [ show (o+10) | Object o <- producer1 @'[Show,Num] ++ producer2]

------------------------------------------------------------------------

upcast :: forall cs ds. cs ⊆ ds => Object ds -> Object cs
upcast (Object (x :: a)) = pickN @cs @ds (Proxy :: Proxy a) (Object x)

using1 :: forall c cs r. c ∈ cs => Object cs -> (forall a. c a => a -> r) -> r
using1 (Object (x :: a)) f = pick1 @c @cs (Proxy :: Proxy a) (f x)

using :: forall cs r. Object cs -> (forall a. All cs a => a -> r) -> r
using (Object x) f = f x

------------------------------------------------------------------------

type c ∈ cs = Pick1 c cs (Index c cs)

class i ~ Index c cs => Pick1 c cs i where
  pick1 :: All cs a => proxy a -> (c a => b) -> b

instance Pick1 c (c ': cs) Z where
  pick1 _ x = x

instance (Index c (d ': ds) ~ S i, Pick1 c ds i) => Pick1 c (d ': ds) (S i) where
  pick1 p = pick1 @c @ds p

------------------------------------------------------------------------

type cs ⊆ ds = PickN cs ds (Image cs ds)

class Image cs ds ~ is => PickN cs ds is where
  pickN :: All ds a => proxy a -> (All cs a => b) -> b

instance PickN '[] ds '[] where
  pickN _ x = x

instance (Pick1 c ds i, PickN cs ds is) => PickN (c ': cs) ds (i ': is) where
  pickN p x = pick1 @c  @ds p
            $ pickN @cs @ds p
            $ x
