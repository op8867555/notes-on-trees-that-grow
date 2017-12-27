{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- 試著用 typeclass 來完成一個 generic 的 trans function

module Trans where

import Expr

class Transformable x0 x1 where
    transXApp :: XApp x0 -> XApp x1
    transXLit :: XLit x0 -> XLit x1
    transXAbs :: XAbs x0 -> XAbs x1
    transXAnn :: XAnn x0 -> XAnn x1
    transXVar :: XVar x0 -> XVar x1
    transXExp :: XExp x0 -> XExp x1


gtrans :: forall x0 x1. Transformable x0 x1 => ExpX x0 -> ExpX x1
gtrans (AppX xv0 a b) = AppX (transXApp @x0 @x1 xv0) (gtrans a) (gtrans b)
gtrans (LitX xv0 x)   = LitX (transXLit @x0 @x1 xv0) x
gtrans (AbsX xv0 x b) = AbsX (transXAbs @x0 @x1 xv0) x (gtrans b)
gtrans (AnnX xv0 x t) = AnnX (transXAnn @x0 @x1 xv0) (gtrans x) t
gtrans (VarX xv0 x)   = VarX (transXVar @x0 @x1 xv0) x
gtrans (ExpX xv0)     = ExpX (transXExp @x0 @x1 xv0)
