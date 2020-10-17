-- ~\~ language=Haskell filename=src/Test/LessArbitrary.hs
-- ~\~ begin <<less-arbitrary.md|src/Test/LessArbitrary.hs>>[0]
{-# language DefaultSignatures     #-}
{-# language FlexibleInstances     #-}
{-# language FlexibleContexts      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language Rank2Types            #-}
{-# language PolyKinds             #-}
{-# language MultiParamTypeClasses #-}
{-# language MultiWayIf            #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language TypeFamilies          #-}
{-# language UndecidableInstances  #-}
{-# language AllowAmbiguousTypes   #-}
{-# language DataKinds             #-}
module Test.LessArbitrary(
    LessArbitrary(..)
  , oneof
  , choose
  , CostGen(..)
  , (<$$$>)
  , ($$$?)
  , currentBudget
  , fasterArbitrary
  , genericLessArbitrary
  , genericLessArbitraryMonoid
  , flatLessArbitrary
  , spend
  , withCost
  , elements
  , forAll
  , sizedCost
  ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Set            as Set
import qualified Data.Vector         as Vector
import qualified Data.Text           as Text
import Control.Monad(replicateM)
import Data.Scientific
import Data.Proxy
import qualified Test.QuickCheck.Gen as QC
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Class
import System.Random(Random)
import GHC.Generics as G
import GHC.Generics as Generic
import GHC.TypeLits
import qualified Test.QuickCheck as QC
import Data.Hashable

-- ~\~ begin <<less-arbitrary.md|costgen>>[0]
newtype Cost = Cost Int 
  deriving (Eq,Ord,Enum,Bounded,Num)

newtype CostGen                               a =
        CostGen {
          runCostGen :: State.StateT Cost QC.Gen a }
  deriving (Functor, Applicative, Monad, State.MonadFix)
-- ~\~ end

-- Mark a costly constructor with this instead of `<$>`
(<$$$>) :: (a -> b) -> CostGen a -> CostGen b
costlyConstructor <$$$> arg = do
  spend 1
  costlyConstructor <$> arg

-- ~\~ begin <<less-arbitrary.md|spend>>[0]
spend :: Cost -> CostGen ()
spend c = CostGen $ State.modify (-c+)
-- ~\~ end

-- ~\~ begin <<less-arbitrary.md|budget>>[0]
($$$?) :: CostGen a
       -> CostGen a
       -> CostGen a
cheapVariants $$$? costlyVariants = do
  budget <- CostGen State.get
  if | budget > (0 :: Cost) -> costlyVariants
     | budget > -10000      -> cheapVariants
     | otherwise            -> error $
       "Recursive structure with no loop breaker."

-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|budget>>[1]
currentBudget :: CostGen Cost
currentBudget = CostGen State.get
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|budget>>[2]
-- unused: loop breaker message type name
type family ShowType k where
  ShowType (D1 ('MetaData name _ _ _) _) = name
  ShowType  other                        = "unknown type"

showType :: forall                      a.
            (Generic                    a
            ,KnownSymbol (ShowType (Rep a)))
         => String
showType  = symbolVal (Proxy :: Proxy (ShowType (Rep a)))
-- ~\~ end


withCost :: Int -> CostGen a -> QC.Gen a
withCost cost gen = runCostGen gen
  `State.evalStateT` Cost cost

-- ~\~ begin <<less-arbitrary.md|generic-instances>>[0]
type family Min m n where
  Min m n = ChooseSmaller (CmpNat m n) m n

type family ChooseSmaller (o::Ordering)
                          (m::Nat)
                          (n::Nat) where 
  ChooseSmaller 'LT m n = m
  ChooseSmaller 'EQ m n = m
  ChooseSmaller 'GT m n = n
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-instances>>[1]
type family Cheapness a :: Nat where
  Cheapness (a :*: b)  =
         Cheapness a + Cheapness b
  Cheapness (a :+: b)  =
    Min (Cheapness a) (Cheapness b)
  Cheapness  U1                      = 0
  -- ~\~ begin <<less-arbitrary.md|flat-types>>[0]
  Cheapness (S1 a (Rec0 Int       )) = 0
  Cheapness (S1 a (Rec0 Scientific)) = 0
  Cheapness (S1 a (Rec0 Double    )) = 0
  Cheapness (S1 a (Rec0 Bool      )) = 0
  Cheapness (S1 a (Rec0 Text.Text )) = 1
  Cheapness (S1 a (Rec0 other     )) = 1
  -- ~\~ end
  Cheapness (K1 a other) = 1
  Cheapness (C1 a other) = 1
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-instances>>[2]
instance GLessArbitrary           f
      => GLessArbitrary (G.C1 c f) where
  gLessArbitrary = G.M1 <$> gLessArbitrary
  cheapest       = G.M1 <$> cheapest

instance GLessArbitrary           f
      => GLessArbitrary (G.S1 c f) where
  gLessArbitrary = G.M1 <$> gLessArbitrary
  cheapest       = G.M1 <$> cheapest
-- ~\~ end

-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[0]
genericLessArbitraryMonoid :: (Generic             a
                              ,GLessArbitrary (Rep a)
                              ,Monoid              a )
                           =>  CostGen             a
genericLessArbitraryMonoid  =
  pure mempty $$$? genericLessArbitrary
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[1]
class GLessArbitrary datatype where
  gLessArbitrary :: CostGen (datatype p)
  cheapest       :: CostGen (datatype p)

genericLessArbitrary :: (Generic             a
                        ,GLessArbitrary (Rep a))
                     =>  CostGen             a
genericLessArbitrary = G.to <$> gLessArbitrary
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[2]
instance GLessArbitrary       f
      => GLessArbitrary (D1 m f) where 
  gLessArbitrary = do
    spend 1
    M1 <$> (cheapest $$$? gLessArbitrary)
  cheapest = M1 <$> cheapest
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[3]
type family SumLen a :: Nat where
  SumLen (a G.:+: b) = SumLen a + SumLen b
  SumLen  a          = 1
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[4]
instance GLessArbitrary G.U1 where
  gLessArbitrary = pure G.U1
  cheapest       = pure G.U1
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[5]
instance (GLessArbitrary  a
         ,GLessArbitrary          b)
      =>  GLessArbitrary (a G.:*: b) where
  gLessArbitrary = (G.:*:) <$> gLessArbitrary
                           <*> gLessArbitrary
  cheapest       = (G.:*:) <$> cheapest
                           <*> cheapest
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[6]
instance  LessArbitrary         c
      => GLessArbitrary (G.K1 i c) where
  gLessArbitrary = G.K1 <$> lessArbitrary
  cheapest       = G.K1 <$> lessArbitrary
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[7]
instance (GLessArbitrary      a
         ,GLessArbitrary                    b
         ,KnownNat (SumLen    a)
         ,KnownNat (SumLen                  b)
         ,KnownNat (Cheapness a)
         ,KnownNat (Cheapness               b)
         )
      => GLessArbitrary      (a Generic.:+: b) where
  gLessArbitrary =
    frequency
      [ (lfreq, L1 <$> gLessArbitrary)
      , (rfreq, R1 <$> gLessArbitrary) ]
    where
      lfreq = fromIntegral
            $ natVal (Proxy :: Proxy (SumLen a))
      rfreq = fromIntegral
            $ natVal (Proxy :: Proxy (SumLen b))
  cheapest =
      if lcheap <= rcheap
         then L1 <$> cheapest
         else R1 <$> cheapest
    where
      lcheap, rcheap :: Int
      lcheap = fromIntegral
             $ natVal (Proxy :: Proxy (Cheapness a))
      rcheap = fromIntegral
             $ natVal (Proxy :: Proxy (Cheapness b))
-- ~\~ end

-- ~\~ begin <<less-arbitrary.md|less-arbitrary-class>>[0]
class LessArbitrary a where
  lessArbitrary :: CostGen a
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|less-arbitrary-class>>[1]
  default lessArbitrary :: (Generic             a
                           ,GLessArbitrary (Rep a))
                        =>  CostGen             a
  lessArbitrary = genericLessArbitrary
-- ~\~ end

instance LessArbitrary Bool where
  lessArbitrary = flatLessArbitrary

instance LessArbitrary Int where
  lessArbitrary = flatLessArbitrary

instance LessArbitrary Integer where
  lessArbitrary = flatLessArbitrary

instance LessArbitrary Double where
  lessArbitrary = flatLessArbitrary

instance LessArbitrary Char where
  lessArbitrary = flatLessArbitrary

instance (LessArbitrary k
         ,LessArbitrary   v)
      => LessArbitrary (k,v) where

instance (LessArbitrary          k
         ,Ord                    k)
      =>  LessArbitrary (Set.Set k) where
  lessArbitrary = Set.fromList <$> lessArbitrary

instance (LessArbitrary              k
         ,Eq                         k
         ,Ord                        k
         ,Hashable                   k 
         ,LessArbitrary                v)
      =>  LessArbitrary (Map.HashMap k v) where
  lessArbitrary =  Map.fromList
               <$> lessArbitrary

instance LessArbitrary Scientific where
  lessArbitrary =
    scientific <$> lessArbitrary
               <*> lessArbitrary

-- ~\~ begin <<less-arbitrary.md|arbitrary-implementation>>[0]
fasterArbitrary :: LessArbitrary a => QC.Gen a
fasterArbitrary = sizedCost lessArbitrary

sizedCost :: CostGen a -> QC.Gen a
sizedCost gen = QC.sized (`withCost` gen)
-- ~\~ end

flatLessArbitrary :: QC.Arbitrary a
              => CostGen a
flatLessArbitrary  = CostGen $ lift QC.arbitrary

instance LessArbitrary                a
      => LessArbitrary (Vector.Vector a) where
  lessArbitrary = Vector.fromList <$> lessArbitrary

-- ~\~ begin <<less-arbitrary.md|lifting-arbitrary>>[0]
instance LessArbitrary  a
      => LessArbitrary [a] where
  lessArbitrary = pure [] $$$? do
    budget <- currentBudget
    len  <- choose (1,fromEnum budget)
    spend $ Cost len
    replicateM   len lessArbitrary

instance QC.Testable          a
      => QC.Testable (CostGen a) where
  property = QC.property
           . sizedCost
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|lifting-arbitrary>>[1]
forAll :: CostGen a -> (a -> CostGen b) -> CostGen b
forAll gen prop = gen >>= prop

oneof   :: [CostGen a] -> CostGen a
oneof [] = error
           "LessArbitrary.oneof used with empty list"
oneof gs = choose (0,length gs - 1) >>= (gs !!)

elements :: [a] -> CostGen a
elements gs = (gs!!) <$> choose (0,length gs - 1)

choose      :: Random  a
            =>        (a, a)
            -> CostGen a
choose (a,b) = CostGen $ lift $ QC.choose (a, b)
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|lifting-arbitrary>>[2]
frequency :: [(Int, CostGen a)] -> CostGen a
frequency [] =
  error $ "LessArbitrary.frequency "
       ++ "used with empty list"
frequency xs
  | any (< 0) (map fst xs) =
    error $ "LessArbitrary.frequency: "
         ++ "negative weight"
  | all (== 0) (map fst xs) =
    error $ "LessArbitrary.frequency: "
         ++ "all weights were zero"
frequency xs0 = choose (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error
    "LessArbitrary.pick used with empty list"
-- ~\~ end

-- ~\~ end
