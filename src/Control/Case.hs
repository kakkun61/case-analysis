{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Control.Case
  ( gfold
  ) where

import Data.Kind    (Constraint, Type)
import Generics.SOP (Generic (Code, from), I (I), NP ((:*)), NS (S, Z), SOP (SOP))

-- | case analysis function.
--
-- >>> :type gfold @Bool
-- gfold @Bool :: Bool -> r -> r -> r
gfold :: forall a r. (GFold (Code a), Generic a) => a -> FunS (Code a) r
gfold = unFun @_ @r . gfold'. from

newtype Fun xss r = Fun { unFun :: FunS xss r }

type FunP :: [Type] -> Type -> Type
type family FunP xs r where
  FunP '[] r = r
  FunP (x ': xs) r = x -> FunP xs r

type FunS :: [[Type]] -> Type -> Type
type family FunS xss r where
  FunS '[] r = r
  FunS (xs ': xss) r = FunP xs r -> FunS xss r

type GFold :: [[Type]] -> Constraint
class GFold xss where
  gfold' :: SOP I xss -> Fun xss r

instance GFold '[] where
  gfold' _ = Fun undefined

instance (Apply xs, Embed xss, GFold xss) => GFold (xs ': xss) where
  gfold' (SOP (S xs)) = constFun (gfold' (SOP xs))
  gfold' (SOP (Z x))  = embed (Fun $ \f -> apply f x)

type Apply :: [Type] -> Constraint
class Apply xs where
  apply :: FunP xs r -> NP I xs -> r

instance Apply '[] where
  apply r _ = r

instance Apply xs => Apply (x ': xs) where
  apply f ((I x) :* xs) = apply (f x) xs

type Embed :: [[Type]] -> Constraint
class Embed xss where
  embed :: Fun (xs ': '[]) r -> Fun (xs ': xss) r

instance Embed '[] where
  embed = id

instance Embed xss => Embed (xs ': xss) where
  embed = flipFun . constFun . embed

flipFun :: Fun (xs ': ys ': xss) r -> Fun (ys ': xs ': xss) r
flipFun f = Fun $ \ys xs -> unFun f xs ys

constFun :: Fun xss r -> Fun (xs ': xss) r
constFun (Fun f) = Fun $ const f
