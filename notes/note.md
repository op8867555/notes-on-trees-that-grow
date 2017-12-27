---
title: Notes on Trees that Grow
---

# Trees That Grow

  * Shayan Najd 跟 Simon Peyton Jones 在 ICFP 2016 講的題目[1] [2]
  * 提出了一個新的作法來解決 擴展、裝飾 AST 的問題
  * [正在整進 GHC 中](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow)

[1]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/trees-that-grow.pdf
[2]: https://www.youtube.com/watch?v=DSWoGdfYt68

## §2.1 裝飾 AST 是個問題

  * 編譯的過程就是在操弄這個樹
    * 標記原始碼位置
    * 標記 namespace
    * 標記 type

  * 操作的過程會一直變動這顆樹
  * compiler 作者面臨兩個選擇：
    * 每一次變動就設計一個新的 data type
    * 設計一個很大的 data type 來包含所有可能
  * GHC 的 HsSyn 有 97 個 data types + 321 個 data constructors
  * HsSyn, TemplateHaskell, haskell-src-exts 都各有一棵

## §2.2 GHC 現在的解法

### Straightforward Parameterisation

~~~~{.haskell}
parse :: String → HsExpr RdrName
rename :: HsExpr RdrName → HsExpr Name
typecheck :: HsExpr Name → HsExpr Id
~~~~

### Extra data constructors

多一個 Constructor 給另一個 pass 用

~~~~{.haskell}
data HsPat id = . . .
  | ConPat    id [Located (HsPat id )]
  | ConPatOut id . . . other fields . . .
~~~~

### Alternating data types

不同的 pass 用不同的 data type

~~~~{.haskell caption="現在的長的不太一樣"}
data HsPat id = ...
  | ConPatIn    (Located (IdP p)) (HsConPatDetails p)
  | ConPatOut   (Located ConLike) ...
~~~~


### Phase-indexed fields

用 type family 來在不同 pass 改變型別

~~~~{.haskell}
data HsExpr id = . . .
  | ExplicitPArr (PostTc id Type) [LHsExpr id ]

type family   PostTc id      a
type instance PostTc RdrName a = ()
type instance PostTc Name    a = ()
type instance PostTc Id      a = a
~~~~

## §3 作者提出的

用了很多 GHC 8 的 extension

~~~~{.haskell}
{-# LANGUAGE TypeFamilies, DataKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs, EmptyCase, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
~~~~


## 原本的樣子

~~~~{.haskell}
data Exp = Lit Integer
         | Var Var
         | Ann Exp Typ
         | Abs Var Exp
         | App Exp Exp
type Var = String
data Typ = Int
         | Fun Typ Typ
~~~~

## 後來的樣子

:::::::::::::: {.columns}
::: {.column width="70%"}
~~~~{.haskell}
data ExpX xi
    = LitX (XLit xi) Integer
    | VarX (XVar xi) Var
    | AnnX (XAnn xi) (ExpX xi) Type
    | AbsX (XAbs xi) Var (ExpX xi)
    | AppX (XApp xi) (ExpX xi) (ExpX xi)
    | ExpX (XExp xi)
type Var = String
data Typ = Int
         | Fun Typ Typ
~~~~
:::
::: {.column width="30%"}
~~~~{.haskell}
type family XLit xi
type family XVar xi
type family XAbs xi
type family XAnn xi
type family XApp xi
type family XExp xi
~~~~
:::
::::::::::::::

  * `XLit xi`...  拿來擴充 field
  * `ExpX` 拿來擴充 constructor

## 怎麼用?

 * 舉例定義一個跟原本一模一樣的
 * 需要一個 `type` 跟一個 `data` 做標籤(extension descriptor)
 * 因為沒有擴充，所以全部的 `XLit xi`... 都填上 `Void`

~~~~{.haskell}
type ExpUD = ExpX UD
data UD

type instance XLit xi = Void
type instance XVar xi = Void
type instance XAbs xi = Void
type instance XAnn xi = Void
type instance XApp xi = Void
type instance XExp xi = Void
~~~~


----

因為用的時候要多帶一個參數，很麻煩

~~~~{.haskell}
incLit :: Exp -> Exp
incLit (Lit i) = Lit (i+1)
incLit e = e

incLit :: ExpUD -> ExpUD
incLit (LitX _ i) = LitX void (i+1)
incLit e = e
~~~~

## §3.2 Pattern Synonyms

用 Pattern Synonyms 可以讓程式好寫一點

~~~~{.haskell}
pattern   LitUD :: Integer -> ExpUD
pattern   LitUD i <- LitX _ i
    where LitUD i = LitX void i

incLit :: ExpUD -> ExpUD
incLit (LitX _ i) = LitX void (i+1)
incLit e = e

incLit' :: ExpUD -> ExpUD
incLit' (LitUD i) = LitUD (i+1)
incLit' e = e
~~~~

## §3.3 加欄位

  * 舉例： 如果要多標 type 的話

    ~~~~{.haskell}
    data Exp' = ... | App Exp Exp
    data Exp  = ... | App Type Exp Exp
    ~~~~

  * 建立一個 `ExpTC = ExpX TC`{.haskell}
  * 就是加在 `(XApp TC)`{.haskell} 上  
    `type instance XApp TC = Type`{.haskell}
  * 加上對應的 Pattern Synonyms

## §3.4 加 Constructor

  * 這邊舉的例子是 Partial Evaluation

    ~~~~{.haskell}
    data Val = ...
    data Exp' = ...
    data Exp  = ... | Val Val
    ~~~~

  * 建立一個 `ExpPE = ExpX PE`{.haskell}
  * 就是加在 `(XExp TC)`{.haskell} 上  
    `type instance XApp PE = Type`{.haskell}
  * 加上對應的 Pattern Synonyms

## §3.5 normal functions on Extended Data Types

  * 應用 Pattern Synonyms 寫起來就跟本來寫得很像
  * 但是 GHC 沒辦法檢查 pattern-matching 有沒有 exhaustive

## §3.6 generic functions

  * 這邊舉了個 printer 的例子，然後先無視 extension fields
  * 做出了一個 generic 的 printer
  * 其實就是傳入處理`(ExpX xi)`{.haskell}的 function
  * 加上對應的 Pattern Synonyms

## §3.7 typeclass

  * 用 type class 來處理前面無視的 extension fields
  * 本來要寫成

    ~~~~{.haskell}
    printE :: (XLit xi -> String)
           -> (XVar xi -> String)
           -> (XAnn xi -> String)
           -> (XAbs xi -> String)
           -> (XApp xi -> String)
           -> (XExp xi -> String)
           -> ExpX xi -> String
    ~~~~

  * 幫 ExpX Xi 建立 Show instance

    ~~~~{.haskell}
    instance ( Show (XLit xi)
             , Show (XVar xi)
             , Show (XAnn xi)
             , Show (XAbs xi)
             , Show (XApp xi)
             , Show (XExp xi)) =>
             Show (ExpX)
    ~~~~

  * 很煩，所以多定義一個 type 來表達

    ~~~~{.haskell}
    type ForallX (phi :: * -> Constraint) xi =
      ( phi (XLit xi) , phi (XVar xi)
      , phi (XAnn xi) , phi (XAbs xi)
      , phi (XApp xi) , phi (XExp xi) )
    ~~~~

  * 就可以幫他

    ~~~~{.haskell}
    instance ForallX Show xi => Show (ExpX xi) where
      show (show_xlit, ....
    ~~~~

  * 還可以用 StandaloneDeriving

    ~~~~{.haskell}
    deriving instance ForallX Show xi => Show (ExpX xi)
    ~~~~

  * 這利用到了

    ~~~~
    Prelude> :kind Show
    Show :: * -> Constraint
    ~~~~

  * `Constraint` 定義在 `GHC.Types` 裡面

## §3.8 改變 constructor

  * 這邊舉的例子是  saturated applications

    ~~~~{.haskell}
    data Exp' = ... | App Exp Exp
    data Exp  = ... | App Exp [Exp]
    ~~~~

  * 建立一個 `ExpSA = ExpX SA`{.haskell}
  * 也是加在 `(XExp SA)`{.haskell} 上  
    `type instance XApp PE = (ExpSA, [ExpSA])`{.haskell}
  * 然後舊的`AppX`{.haskell} 就用不要 export 給 user

## §3.9 type parameters

  * 前面講的 `data Exp`{.haskell} 沒有 type parameter
  * 這邊介紹有 `data Exp a`{.haskell} 的場合
  * 舉的例子是 Let insertion


## §3.10 Existentials & GADT

### 跟前面一樣的做法
  * 在每個 Constructor 都多傳一個 `XLit`之類的 type
  * 但是要把 local type variables 也抓出來

### typeclass

  * 做了一個 data type 來支援 type class

    ~~~~{.haskell}
    data Proof phi a where
        Proof :: phi a => Proof phi a
    ~~~~

  * 接著就跟之前一樣做一個`ExpSh a = ExpX Sh a`{.haskell}
  * 定義 `type instance XApp Sh a b = Proof Show a`{.haskell}
  * 也可以做一個 local variable 用的

    ~~~~{.haskell}
    data Exists f where
        Exists :: f a -> Exists f
    ~~~~


## §3.11 變種

  * extension field 不用放在全部的 constructors 上

## §3.12 缺點

  * 效率變低：多了 extension field
  * pattern-match 沒辦法檢查 exhaustive
  * boilerplate：每個改變都要加上很多行

## §4.1 加上 type parameter

  * 加在 extension descriptor 上面

    ~~~~{.haskell}
    type ExpAn a = ExpX (An a)
    data An a
    type instance XLit (An a) = a
    ....
    type instance XExp (An a) = Void
    ~~~~

## §4.2 hierarchy of Extension Descriptors

  * Extension Descriptors 可以訂成樹狀
  * 表達 compiler 間的不同階段

# discussion and related works

  * 跟 expression problem 比起來多了 field 要加上
  * Generic programming is a plus, not a must:  
  Our primary goal is to reuse the data type declaration itself, rather than to re-use existing functions that operate over that type.
  * AST 是有宣告的
  * 這個解法可以在 GHC 上運作

# reference
