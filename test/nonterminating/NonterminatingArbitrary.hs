-- ~\~ language=Haskell filename=test/nonterminating/NonterminatingArbitrary.hs
-- ~\~ begin <<less-arbitrary.md|test/nonterminating/NonterminatingArbitrary.hs>>[0]
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
-- ~\~ begin <<less-arbitrary.md|tree-type-typical-arbitrary>>[0]
instance Arbitrary       a
      => Arbitrary (Tree a) where
  arbitrary = oneof [Leaf   <$> arbitrary
                    ,Branch <$> arbitrary
                    ]
-- ~\~ end
otherLaws = []
-- ~\~ begin <<less-arbitrary.md|test-file-laws>>[0]

main :: IO ()
main = do
  lawsCheckMany
    [("Tree",
      [arbitraryLaws (Proxy :: Proxy (Tree Int))
      ,eqLaws        (Proxy :: Proxy (Tree Int))
      ] <> otherLaws)]
-- ~\~ end
-- ~\~ end
