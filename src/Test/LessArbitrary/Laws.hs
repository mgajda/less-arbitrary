-- ~\~ language=Haskell filename=src/Test/LessArbitrary/Laws.hs
-- ~\~ begin <<less-arbitrary.md|src/Test/LessArbitrary/Laws.hs>>[0]
{-# language DataKinds             #-}
{-# language FlexibleInstances     #-}
{-# language Rank2Types            #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# language AllowAmbiguousTypes   #-}
module Test.LessArbitrary.Laws(
      lessArbitraryLaws
    ) where

import Data.Proxy
import Test.QuickCheck(Gen, property)
import Test.QuickCheck.Classes(Laws(..))
import Test.LessArbitrary
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict(HashMap)

-- ~\~ begin <<less-arbitrary.md|less-arbitrary-laws>>[0]
lessArbitraryLaws :: forall        s a.
                     LessArbitrary s a
                  =>                (a -> Bool)
                  -> Laws
lessArbitraryLaws cheapestPred =
    Laws "LessArbitrary"
         [("always selects cheapest",
           property $
             (prop_alwaysCheapest @s @a) cheapestPred)]

prop_alwaysCheapest :: forall s a.
                       LessArbitrary s a
                    =>                (a -> Bool)
                    -> Gen                  Bool
prop_alwaysCheapest cheapestPred =
  cheapestPred <$> (withCost @s @a) 0 lessArbitrary
-- ~\~ end
-- ~\~ end
