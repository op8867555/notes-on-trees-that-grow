{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Printer where

import Expr
import GHC.Types (Constraint)


printT :: Type -> String
printT Int = "Int"
printT (Fun a b) = "(" ++ printT a ++ ") ->" ++ printT b

-- 跟 paper 不同，直接用了 Show typeclass
printE :: Show (ExpX xi) => ExpX xi -> String
printE (LitX _ i) = show i
printE (VarX _ x) = x
printE (AnnX _ m a) = "(" ++ printE m ++ ")::(" ++ printT a ++ ")"
printE (AbsX _ x n) = "λ" ++ x ++ "." ++ printE n
printE (AppX _ l m) = "(" ++ printE l ++ ")(" ++ printE m ++ ")"
printE exp@(ExpX _) = show exp


type ForallX (phi :: * -> Constraint) xi =
    ( phi (XLit xi)
    , phi (XVar xi)
    , phi (XAnn xi)
    , phi (XAbs xi)
    , phi (XApp xi)
    , phi (XExp xi) )
