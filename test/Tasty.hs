module Main (main) where

import Control.Monad (join)
import Data.Coerce (coerce)
import Data.Either.Validation (Validation (..), validationToEither)
import Data.Functor.Foldable (Base, Corecursive, Recursive, cata)
import qualified Dhall as D
import qualified Dhall.Core as D
import Dhall.Deriving.Recursive (RecADT (RecADT))
import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Recursive, Corecursive)
  deriving (D.FromDhall, D.ToDhall) via (RecADT (Tree a))

type instance Base (Tree a) = TreeF a

data TreeF a b = LeafF a | BranchF b b
  deriving stock (Functor, Generic)
  deriving anyclass (D.FromDhall, D.ToDhall)

main :: IO ()
main = do
  let fld = ("(./test/tree.dhall)." <>)
  dhallTree <- D.inputExpr $ fld "exampleTree"
  dhallLeafList :: Tree Natural -> [Natural] <- D.input D.auto $ fld "leafList"

  defaultMain . testGroup "tests" $
    [ testCase "Haskell → Dhall" $
        JExpr dhallTree @=? JExpr do D.embed D.inject haskellTree,
      testCase "Dhall → Haskell" $
        assertValidation haskellTree $ D.extract D.auto dhallTree,
      testProperty "Dhall function = Haskell function" $ property do
        t <- forAll genTree
        dhallLeafList t === haskellLeafList t,
      testProperty "roundtrip Haskell → Dhall → Haskell" $ property do
        t <- forAll genTree
        let roundtrip = D.extract D.auto . D.embed D.inject
        t' <- evalEither . validationToEither $ roundtrip t
        t === t',
      testProperty "roundtrip Dhall → Haskell → Dhall" $ property do
        t <- D.embed D.inject <$> forAll genTree
        let roundtrip = fmap (D.embed D.inject) . D.extract (D.auto @(Tree Natural))
        t' <- evalEither . validationToEither $ roundtrip t
        JExpr t === JExpr t'
    ]
  where
    haskellTree :: Tree Natural
    haskellTree = Branch (Branch (Branch (Leaf 0) (Leaf 5)) (Branch (Leaf 4) (Leaf 3))) (Leaf 6)

    haskellLeafList :: Tree a -> [a]
    haskellLeafList = cata \case
      LeafF a -> [a]
      BranchF t t' -> t <> t'

    genTree :: MonadGen m => m (Tree Natural)
    genTree =
      Gen.recursive
        Gen.choice
        [Leaf <$> Gen.integral_ (Range.linear 0 100)]
        [join Gen.subterm2 genTree Branch]

newtype JExpr s a = JExpr (D.Expr s a)
  deriving newtype (Show)

instance Eq a => Eq (JExpr s a) where
  (==) = coerce (D.judgmentallyEqual @a @s @s)

assertValidation :: (HasCallStack, Show e, Show a, Eq a) => a -> Validation e a -> Assertion
assertValidation a (Success a') = a @=? a'
assertValidation _ (Failure e) = assertFailure $ show e
