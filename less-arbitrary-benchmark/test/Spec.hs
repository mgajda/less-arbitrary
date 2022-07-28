{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Criterion.Main

import Control.Monad
import Control.Monad.Par.Class

-- For generic derivations:
import GHC.Generics

-- Traditional QC
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Arbitrary as QC

-- Less arbitrary
import Test.LessArbitrary

-- Feat -- option enumeration
import Test.Feat
import Test.Feat.Enumerate
import Test.Feat.Modifiers
import Test.Feat.Access
import Control.Enumerable

data Tree =
            Leaf   Int
          | Branch Tree Tree
  deriving (Show, Eq, Ord, Generic)
-- FIXME: Performance may depend on the order of constructors?

instance Enumerable Tree where
  enumerate =
    datatype [
      pay $ c2 Branch
    ,       c1 Leaf
    ]

-- | No generic implementation
instance QC.Arbitrary Tree where
  arbitrary = tree
  shrink    = QC.genericShrink

instance NFData Tree where

-- | Sized generator of arbitrary for recursive datatype.
--   From the tutorial: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
tree :: QC.Gen Tree
tree = QC.sized tree'
  where
    tree' :: Int -> QC.Gen Tree
    tree' 0       = liftM  Leaf   QC.arbitrary
    tree' n | n>0 = 
        QC.oneof  [ liftM  Leaf   QC.arbitrary,
                    liftM2 Branch subtree subtree]
      where subtree = tree' (n `div` 2)

treeSize (Leaf _    ) = 1
treeSize (Branch a b) = treeSize a + treeSize b

instance LessArbitrary () Tree

testVectorSize = 1000
testSize = 1000

sampleFeat :: Int -> QC.Gen Tree
sampleFeat aSize = selectTree aSize <$> QC.choose (c `div` 2, c)
  where
    c = count aSize
    count n = fst $ treeValues !! n

    enumTrees :: Enumerate Tree
    enumTrees = global

    selectTree = selectWith enumTrees

    treeValues :: [(Integer, [Tree])]
    treeValues = valuesWith enumTrees

main :: IO ()
main = defaultMain [
         bgroup "tree" [
           bench "arbitrary" $
             nfIO            $
             QC.generate     $
             QC.vectorOf testVectorSize $
             (QC.arbitrary :: QC.Gen Tree)
         , bench "lessArbitrary" $
             nfIO            $
             QC.generate     $
             QC.vectorOf testVectorSize $
             sizedCost       $
             (lessArbitrary :: CostGen () Tree)
         , bench "feat"      $
             nfIO            $
             QC.generate     $
             QC.vectorOf testVectorSize $
             QC.sized        $
             sampleFeat
         ]
       ]


