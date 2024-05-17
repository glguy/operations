{-# Language ExistentialQuantification #-}
{-# Language QuantifiedConstraints #-}
{-# Language MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# Language RankNTypes #-}
{-# Language ConstraintKinds, DataKinds #-}
{-# Language TypeOperators #-}
{-# Language InstanceSigs #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances, UndecidableSuperClasses #-}
{-# Language TypeApplications, AllowAmbiguousTypes #-}

module Object
  ( Object(Object)
  , type (⊆)

  , upcast

  , using

  , type (&)
  , Null

  ) where

import Data.Kind
import Control.Category (id, (.))
import Prelude hiding (id, (.))

data Object c = forall a. c a => Object a

instance (Show ⊆ cs) => Show (Object cs) where
  show o = objectShowsPrec 11 o ""
  showsPrec = objectShowsPrec

  showList [] s = "[]" ++ s
  showList (x:xs) s = '[' : objectShowsPrec 0 x (foldr aux (']':s) xs)
    where
      aux :: Object cs -> ShowS
      aux y ys = ',' : objectShowsPrec 0 y ys

objectShowsPrec :: (Show ⊆ cs) => Int -> Object cs -> ShowS
objectShowsPrec p (Object o)
     = showParen (p >= 11)
     $ showString "Object "
     . showsPrec 11 o

upcast :: forall cs ds. cs ⊆ ds => Object ds -> Object cs
upcast o = using @cs o Object

using :: forall c cs r. c ⊆ cs => Object cs -> (forall a. c a => a -> r) -> r
using (Object o) f = f o

------------------------------------------------------------------------

type (cs :: Type -> Constraint) ⊆ (ds :: Type -> Constraint) = (forall a. ds a => cs a) :: Constraint

class    (c1 a, c2 a) => (c1 & c2) a
instance (c1 a, c2 a) => (c1 & c2) a

class    Null a
instance Null a
