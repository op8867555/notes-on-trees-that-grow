{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- UD 的變種：
--   * 用 `()` 代替 `Void` （為了 Show typeclass）
module UD2 where

import Expr
import Printer

type ExpUD = ExpX UD
data UD

type instance XLit UD = ()
type instance XVar UD = ()
type instance XAbs UD = ()
type instance XAnn UD = ()
type instance XApp UD = ()
type instance XExp UD = ()

pattern   LitUD :: Integer -> ExpUD
pattern   LitUD i <- LitX _ i
    where LitUD i = LitX () i

pattern   AppUD :: ExpUD -> ExpUD -> ExpUD
pattern   AppUD a b = AppX () a b

pattern   VarUD :: Var -> ExpUD
pattern   VarUD v <- VarX _ v
    where VarUD v =  VarX () v

pattern   AnnUD :: ExpUD -> Type -> ExpUD
pattern   AnnUD t x <- AnnX _ t x
    where AnnUD t x =  AnnX () t x

pattern   AbsUD :: Var -> ExpUD -> ExpUD
pattern   AbsUD n b <- AbsX _ n b
    where AbsUD n b =  AbsX () n b

incLit :: ExpUD -> ExpUD
incLit (LitX _ i) = LitX () (i+1)
incLit e = e

incLit' :: ExpUD -> ExpUD
incLit' (LitUD i) = LitUD (i+1)
incLit' e = e

deriving instance ForallX Show UD => Show (ExpX UD)
