{-# LANGUAGE MultiParamTypeClasses #-}
module Test where

import UD2
import TC
import Expr

x0 = LitUD 1
x1 = (AnnTC (LitTC 1) Int)
x2 = (AppTC Int (AbsTC "x" (VarTC "x")) (LitTC 1))

-- TODO: 實驗用， AppTC 的時做事錯的
transform :: ExpUD -> ExpTC
transform (AppUD a b) = AppTC Int (transform a) (transform b)
transform (LitUD x)   = LitTC x
transform (AbsUD x m) = AbsTC x (transform m)
transform (AnnUD e t) = AnnTC (transform e) t
transform (VarUD n)   = VarTC n
