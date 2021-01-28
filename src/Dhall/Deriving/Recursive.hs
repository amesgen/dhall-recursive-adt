{-# LANGUAGE UndecidableInstances #-}

-- | Convert /recursive/ ADTs from and to Dhall with only /some/ boilerplate.
--
-- This module is intended to be used with
-- [@DerivingVia@](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-DerivingVia).
--
-- Click "Dhall.Deriving.Recursive#example" for a compact example.
module Dhall.Deriving.Recursive
  ( -- * Walkthrough
    -- $walkthrough

    -- * Compact example
    -- $example

    -- * The @DerivingVia@ newtype
    RecADT (..),

    -- * Utilities
    -- $utilities
    RecFromDhall,
    recAuto,
    recAutoWith,
    RecToDhall,
    recInject,
    recInjectWith,
  )
where

import Data.Fix (Fix)
import Data.Functor.Contravariant (contramap)
import Data.Functor.Foldable (Base, Corecursive, Recursive, refix)
import qualified Dhall as D

-- | Intended to be used with @DerivingVia@, as explained in the module documentation.
--
-- [@a@]: your (recursive) ADT

-- We could work without UndecidableInstances if we added a type parameter f (~ Base a).
-- This wouldn't be a disaster, but it is a bit less convenient.
newtype RecADT a = RecADT {unRecADT :: a}

instance RecFromDhall a => D.FromDhall (RecADT a) where
  autoWith = fmap RecADT . recAutoWith

instance RecToDhall a => D.ToDhall (RecADT a) where
  injectWith = contramap unRecADT . recInjectWith

type RecFromDhall a = (D.FromDhall (Base a (D.Result (Base a))), Corecursive a)

recAuto :: RecFromDhall a => D.Decoder a
recAuto = recAutoWith D.defaultInputNormalizer

recAutoWith :: forall a. RecFromDhall a => D.InputNormalizer -> D.Decoder a
recAutoWith = fmap refix . D.autoWith @(Fix (Base a))

type RecToDhall a = (D.ToDhall (Base a (D.Result (Base a))), Recursive a)

recInject :: RecToDhall a => D.Encoder a
recInject = recInjectWith D.defaultInputNormalizer

recInjectWith :: forall a. RecToDhall a => D.InputNormalizer -> D.Encoder a
recInjectWith = contramap refix . D.injectWith @(Fix (Base a))

-- $setup
--
-- >>> import Numeric.Natural (Natural)
-- >>> import GHC.Generics (Generic)
-- >>> import Dhall (FromDhall, ToDhall)
-- >>> :set -XDerivingVia
-- >>> :set -XDeriveAnyClass
-- >>> :set -XDeriveFunctor
-- >>> :set -XDeriveFoldable
-- >>> :set -XDeriveTraversable
-- >>> :set -XDeriveGeneric
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes
-- >>> :set -XStandaloneDeriving
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeApplications
-- >>> :set -XTypeFamilies

-- $walkthrough
--
-- Out of the box, [dhall](https://hackage.haskell.org/package/dhall) can not derive 'D.FromDhall'/'D.ToDhall'
-- for recursive ADTs like this one
--
-- >>> :{
-- data Expr = Lit Natural
--           | Add Expr Expr
--           | Mul Expr Expr
--  deriving stock Show
-- :}
--
-- As Dhall has no recursion by design, it might be non-obvious how to encode this type in Dhall. Luckily,
-- the Church\/Böhm-Berarducci encoding exists, which is essentially a fancy name for the Go4
-- [Visitor pattern](https://en.wikipedia.org/wiki/Visitor_pattern), as explained in
-- [this blog post](https://www.haskellforall.com/2021/01/the-visitor-pattern-is-essentially-same.html).
-- Also make sure check out the
-- [official How-to on translating recursive code to Dhall](https://docs.dhall-lang.org/howtos/How-to-translate-recursive-code-to-Dhall.html).
--
-- Explicitly, consider the /non-recursive/ ADT
--
-- >>> :{
-- data ExprF a = LitF Natural
--              | AddF a a
--              | MulF a a
-- :}
--
-- For any type @a@, this is a non-recursive sum type, which we can directly translate to a Dhall union type:
--
-- > let ExprF = λ(a : Type) → < LitF : Natural | AddF : { _1 : a, _2 : a } | MulF : { _1 : a, _2 : a } >
--
-- We can derive 'D.FromDhall'\/'D.ToDhall' for @ExprF a@. Let us use the default 'GHC.Generics.Generic'-powered
-- version here, but we could also use e.g. "Dhall.Deriving" for customization (we use
-- [@StandaloneDeriving@](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-StandaloneDeriving)
-- here to be able to define the instances one by one).
--
-- >>> deriving stock    instance                Generic   (ExprF a)
-- >>> deriving anyclass instance FromDhall a => FromDhall (ExprF a)
-- >>> deriving anyclass instance ToDhall a   => ToDhall   (ExprF a)
--
-- Right now, @Expr@ and @ExprF@ are completely unrelated. The open type family 'Base' and the type classes
-- 'Recursive' and 'Corecursive' from [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes)
-- are exactly what we need.
--
-- We can derive 'Recursive' and 'Corecursive' for @Expr@ as long as we derive 'GHC.Generics.Generic' for it, and
-- define @'Base' Expr ≔ ExprF@ as well as derive 'Functor' for @ExprF@.
--
-- >>> import Data.Functor.Foldable (Base, Recursive, Corecursive)
-- >>> type instance Base Expr = ExprF
-- >>> deriving stock    instance Functor     ExprF
-- >>> deriving stock    instance Generic     Expr
-- >>> deriving anyclass instance Recursive   Expr
-- >>> deriving anyclass instance Corecursive Expr
--
-- With this setup, we can finally derive 'D.FromDhall'\/'D.ToDhall' for @Expr@:
--
-- >>> deriving via RecADT Expr instance FromDhall Expr
-- >>> deriving via RecADT Expr instance ToDhall   Expr
--
-- To see how this works, consider this expression representing @(1 + 2) * (3 + 4)@:
--
-- >>> expr = Mul (Add (Lit 1) (Lit 2)) (Add (Lit 3) (Lit 4))
-- >>> import qualified Dhall as D
-- >>> encodedExpr = D.embed D.inject expr
--
-- In Dhall, we can construct an @Expr@ like this:
--
-- >>> import qualified Dhall as D
-- >>> import NeatInterpolation
-- >>> :{
-- dhallExpr <- D.inputExpr [trimming|
--   let ExprF = λ(a : Type) → < LitF : Natural | AddF : { _1 : a, _2 : a } | MulF : { _1 : a, _2 : a } >
--   let Expr  = ∀(a : Type) → (ExprF a → a) → a
--   --
--   let Lit : Natural → Expr     = λ(n : Natural) →
--     λ(a : Type) → λ(make : ExprF a → a) → make ((ExprF a).LitF n)
--   --
--   let Add : Expr → Expr → Expr = λ(x : Expr) → λ(y : Expr) →
--     λ(a : Type) → λ(make : ExprF a → a) → make ((ExprF a).AddF { _1 = x a make, _2 = y a make })
--   --
--   let Mul : Expr → Expr → Expr = λ(x : Expr) → λ(y : Expr) →
--     λ(a : Type) → λ(make : ExprF a → a) → make ((ExprF a).MulF { _1 = x a make, _2 = y a make })
--   --
--   in  Mul (Add (Lit 1) (Lit 2)) (Add (Lit 3) (Lit 4))
--   |]
-- :}
--
-- In fact, we have
--
-- >>> import Dhall.Core (judgmentallyEqual)
-- >>> encodedExpr `judgmentallyEqual` dhallExpr
-- True
-- >>> D.extract @Expr D.auto dhallExpr
-- Success (Mul (Add (Lit 1) (Lit 2)) (Add (Lit 3) (Lit 4)))

-- $example
-- #example#
--
-- We can still shorten the boilerplate a bit using 'Data.Functor.Foldable.TH.makeBaseFunctor'.
--
-- >>> import Data.Functor.Foldable.TH (makeBaseFunctor)
-- >>> :{
-- data Tree a = Leaf a | Branch (Tree a) (Tree a)
-- makeBaseFunctor ''Tree
-- :}
--
-- >>> deriving stock    instance                               Generic   (TreeF a b)
-- >>> deriving anyclass instance (FromDhall a, FromDhall b) => FromDhall (TreeF a b)
-- >>> deriving anyclass instance (ToDhall   a, ToDhall   b) => ToDhall   (TreeF a b)
--
-- >>> deriving via RecADT (Tree a) instance FromDhall a => FromDhall (Tree a)
-- >>> deriving via RecADT (Tree a) instance ToDhall   a => ToDhall   (Tree a)
--
-- The test suite contains an expanded example of this situation.

-- $utilities
--
-- If you don't want to derive 'D.FromDhall'/'D.ToDhall' with @DerivingVia@, use these functions instead.
