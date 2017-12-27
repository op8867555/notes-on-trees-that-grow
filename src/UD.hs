{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module UD where

import Expr
import Data.Void

type ExpUD = ExpX UD
data UD

type instance XLit UD = Void
type instance XVar UD = Void
type instance XAbs UD = Void
type instance XAnn UD = Void
type instance XApp UD = Void
type instance XExp UD = Void

pattern   LitUD :: Integer -> ExpUD
pattern   LitUD i <- LitX _ i
    where LitUD i = LitX void i

void :: Void
void = error "void"

incLit :: ExpUD -> ExpUD
incLit (LitX _ i) = LitX void (i+1)
incLit e = e

incLit' :: ExpUD -> ExpUD
incLit' (LitUD i) = LitUD (i+1)
incLit' e = e

