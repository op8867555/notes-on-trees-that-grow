{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Expr where

type Var = String

data Type = Int
          | Fun Type Type
    deriving (Eq, Show)

data ExpX xi
    = LitX (XLit xi) Integer
    | VarX (XVar xi) Var
    | AnnX (XAnn xi) (ExpX xi) Type
    | AbsX (XAbs xi) Var (ExpX xi)
    | AppX (XApp xi) (ExpX xi) (ExpX xi)
    | ExpX (XExp xi)

type family XLit xi
type family XVar xi
type family XAbs xi
type family XAnn xi
type family XApp xi
type family XExp xi
