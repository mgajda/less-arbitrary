{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Criterion.Main

import Control.Monad
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

data Tree = Branch Tree Tree
          | Leaf   Int
  deriving (Show, Eq, Ord, Generic)

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

main :: IO ()
main = defaultMain [
         bgroup "tree" [
         ]
       ]


