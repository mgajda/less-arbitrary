-- ~\~ language=Haskell filename=src/Test/LessArbitrary/Cost.hs
-- ~\~ begin <<less-arbitrary.md|src/Test/LessArbitrary/Cost.hs>>[0]
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.LessArbitrary.Cost where

-- ~\~ begin <<less-arbitrary.md|cost>>[0]
newtype Cost = Cost Int
  deriving (Eq,Ord,Enum,Bounded,Num)
-- ~\~ end
-- ~\~ end
