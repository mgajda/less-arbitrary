-- ~\~ language=Haskell filename=test/less/LessArbitrary.hs
-- ~\~ begin <<less-arbitrary.md|test/less/LessArbitrary.hs>>[0]
-- ~\~ begin <<less-arbitrary.md|test-file-header>>[0]
{-# language FlexibleInstances     #-}
{-# language InstanceSigs          #-}
{-# language Rank2Types            #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# language AllowAmbiguousTypes   #-}
{-# language DeriveGeneric         #-}
module Main where

import           Data.Proxy
import           Test.QuickCheck
import qualified Test.QuickCheck.Gen as QC
import qualified GHC.Generics as Generic
import           Test.QuickCheck.Classes

import           Test.LessArbitrary
import           Test.Arbitrary.Laws
import           Test.LessArbitrary.Laws

-- ~\~ begin <<less-arbitrary.md|tree-type>>[0]
data Tree        a =
    Leaf         a
  | Branch [Tree a]
  deriving (Eq,Show,Generic.Generic)
-- ~\~ end
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|test-less-arbitrary-version>>[0]
instance (Arbitrary             a
         ,LessArbitrary s       a)
      =>  LessArbitrary s (Tree a) where
  lessArbitrary = genericLessArbitrary

instance (LessArbitrary () (Tree a)
         ,Arbitrary              a)
      => Arbitrary         (Tree a) where
  arbitrary = fasterArbitrary @() @(Tree a)
  shrink    = recursivelyShrink
-- ~\~ end

-- ~\~ begin <<less-arbitrary.md|test-file-laws>>[0]

main :: IO ()
main = do
  lawsCheckMany
    [("Tree",
      [arbitraryLaws (Proxy :: Proxy (Tree Int))
      ,eqLaws        (Proxy :: Proxy (Tree Int))
      ] <> otherLaws)]
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|less-arbitrary-check>>[0]
otherLaws :: [Laws]
otherLaws = [lessArbitraryLaws @() isLeaf]
  where
    isLeaf :: Tree Int -> Bool
    isLeaf (Leaf   _) = True
    isLeaf (Branch _) = False
-- ~\~ end
-- ~\~ end
