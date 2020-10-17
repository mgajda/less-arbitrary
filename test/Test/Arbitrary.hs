-- ~\~ language=Haskell filename=test/Test/Arbitrary.hs
-- ~\~ begin <<less-arbitrary.md|test/Test/Arbitrary.hs>>[0]
{-# language DataKinds             #-}
{-# language FlexibleInstances     #-}
{-# language Rank2Types            #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# language AllowAmbiguousTypes   #-}
module Test.Arbitrary(
      arbitraryLaws
    ) where

import Data.Proxy
import Test.QuickCheck
import Test.QuickCheck.Classes
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict(HashMap)

-- ~\~ begin <<less-arbitrary.md|arbitrary-laws>>[0]
shrinkCheck :: forall    term.
              (Arbitrary term
              ,Eq        term)
            =>           term
            -> Bool
shrinkCheck term =
  term `notElem` shrink term

arbitraryLaws :: forall    ty.
                (Arbitrary ty
                ,Show      ty
                ,Eq        ty)
              => Proxy     ty
              -> Laws
arbitraryLaws (Proxy :: Proxy ty) =
  Laws "arbitrary"
       [("does not shrink to itself",
         property (shrinkCheck :: ty -> Bool))]
-- ~\~ end
-- ~\~ end
