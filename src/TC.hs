{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

-- for show
{-# LANGUAGE UndecidableInstances #-}

module TC where

import Expr

import Printer

type ExpTC = ExpX TC
data TC

type instance XLit TC = ()
type instance XVar TC = ()
type instance XAbs TC = ()
type instance XAnn TC = ()
type instance XApp TC = Type
type instance XExp TC = ()

pattern   AppTC :: Type -> ExpTC -> ExpTC -> ExpTC
pattern   AppTC typ a b = AppX typ a b

pattern   LitTC :: Integer -> ExpTC
pattern   LitTC x <- LitX _ x
    where LitTC x =  LitX () x

pattern   VarTC :: Var -> ExpTC
pattern   VarTC v <- VarX _ v
    where VarTC v =  VarX () v

pattern   AnnTC :: ExpTC -> Type -> ExpTC
pattern   AnnTC t x <- AnnX _ t x
    where AnnTC t x =  AnnX () t x

pattern   AbsTC :: Var -> ExpTC -> ExpTC
pattern   AbsTC n b <- AbsX _ n b
    where AbsTC n b =  AbsX () n b

check :: ExpTC -> [(Var, Type)] -> Type -> Bool
check (LitTC _) _env Int = True
check (VarTC x) env c = case lookup x env of
                          Just t -> c == t
                          Nothing -> False
check (AnnTC m a) env c = a == c && check m env c
check (AbsTC x n) env (Fun a b) = check n ((x, a):env) b
check (AppTC a l m) env c = check l env (Fun a c) && check m env a
check _ _ _ = False

deriving instance ForallX Show TC => Show (ExpX TC)
