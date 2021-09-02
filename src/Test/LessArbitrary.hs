-- ~\~ language=Haskell filename=src/Test/LessArbitrary.hs
-- ~\~ begin <<less-arbitrary.md|src/Test/LessArbitrary.hs>>[0]
{-# language DefaultSignatures          #-}
{-# language FlexibleInstances          #-}
{-# language FlexibleContexts           #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language InstanceSigs               #-}
{-# language Rank2Types                 #-}
{-# language PolyKinds                  #-}
{-# language MultiParamTypeClasses      #-}
{-# language MultiWayIf                 #-}
{-# language ScopedTypeVariables        #-}
{-# language TypeApplications           #-}
{-# language TypeOperators              #-}
{-# language TypeFamilies               #-}
{-# language TupleSections              #-}
{-# language UndecidableInstances       #-}
{-# language AllowAmbiguousTypes        #-}
{-# language DataKinds                  #-}
module Test.LessArbitrary(
    LessArbitrary(..)
  , oneof
  , choose
  , budgetChoose
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
  , StartingState(..)
  ) where

import qualified Data.HashMap.Strict        as Map
import qualified Data.Set                   as Set
import qualified Data.Vector                as Vector
import qualified Data.Text                  as Text
import           Control.Monad (replicateM)
import           Data.Scientific
import           Data.Proxy
import qualified Test.QuickCheck.Gen        as QC
import qualified Control.Monad.State.Strict as State
import           Control.Arrow (first, second)
import           Control.Monad.Trans.Class
import           System.Random (Random)
import           GHC.Generics    as G
import           GHC.Generics    as Generic
import           GHC.TypeLits
import           GHC.Stack
import qualified Test.QuickCheck as QC
import           Data.Hashable

import Test.LessArbitrary.Cost

-- ~\~ begin <<less-arbitrary.md|starting-state>>[0]
class StartingState s where
  startingState :: s

instance StartingState () where
  startingState = ()
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|costgen>>[0]
newtype CostGen s a =
        CostGen {
          runCostGen :: State.StateT (Cost, s) QC.Gen a
        }
  deriving (Functor, Applicative, Monad, State.MonadFix)

instance State.MonadState        s  (CostGen s) where
  state :: forall s a.
           (s -> (a, s)) -> CostGen s a
  state nestedMod = CostGen $ State.state mod
    where
      mod :: (Cost, s) -> (a, (Cost, s))
      mod (aCost, aState) = (result, (aCost, newState))
        where
          (result, newState) = nestedMod aState
-- ~\~ end

-- Mark a costly constructor with this instead of `<$>`
(<$$$>) :: (a -> b) -> CostGen s a -> CostGen s b
costlyConstructor <$$$> arg = do
  spend 1
  costlyConstructor <$> arg

-- ~\~ begin <<less-arbitrary.md|spend>>[0]
spend :: Cost -> CostGen s ()
spend c = do
  CostGen $ State.modify (first (-c+))
  checkBudget
-- ~\~ end

-- ~\~ begin <<less-arbitrary.md|budget>>[0]
($$$?) :: HasCallStack
       => CostGen s a
       -> CostGen s a
       -> CostGen s a
cheapVariants $$$? costlyVariants = do
  budget <- fst <$> CostGen State.get
  if | budget > (0 :: Cost) -> costlyVariants
     | budget > -10000      -> cheapVariants
     | otherwise            -> error $
       "Recursive structure with no loop breaker."
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|budget>>[1]
checkBudget :: HasCallStack => CostGen s ()
checkBudget = do
  budget <- fst <$> CostGen State.get
  if budget < -10000
    then error "Recursive structure with no loop breaker."
    else return ()
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|budget>>[2]
currentBudget :: CostGen s Cost
currentBudget = fst <$> CostGen State.get
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|budget>>[3]
-- unused: loop breaker message type name
-- FIXME: use to make nicer error message
type family ShowType k where
  ShowType (D1 ('MetaData name _ _ _) _) = name
  ShowType  other                        = "unknown type"

showType :: forall                      a.
            (Generic                    a
            ,KnownSymbol (ShowType (Rep a)))
         => String
showType  = symbolVal (Proxy :: Proxy (ShowType (Rep a)))
-- ~\~ end


withCost :: forall        s a.
            StartingState s
         => Int
         -> CostGen       s a
         -> QC.Gen          a
withCost cost gen = withCostAndState cost startingState gen

withCostAndState :: Int -> s -> CostGen s a -> QC.Gen a
withCostAndState cost state gen = runCostGen gen
  `State.evalStateT` (Cost cost, state)

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
instance GLessArbitrary s         f
      => GLessArbitrary s (G.C1 c f) where
  gLessArbitrary = G.M1 <$> gLessArbitrary
  cheapest       = G.M1 <$> cheapest

instance GLessArbitrary s         f
      => GLessArbitrary s (G.S1 c f) where
  gLessArbitrary = G.M1 <$> gLessArbitrary
  cheapest       = G.M1 <$> cheapest
-- ~\~ end

-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[0]
genericLessArbitraryMonoid :: (Generic               a
                              ,GLessArbitrary s (Rep a)
                              ,Monoid                a )
                           =>  CostGen        s      a
genericLessArbitraryMonoid  =
  pure mempty $$$? genericLessArbitrary
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[1]
class GLessArbitrary s datatype where
  gLessArbitrary :: CostGen s (datatype p)
  cheapest       :: CostGen s (datatype p)

genericLessArbitrary :: (Generic               a
                        ,GLessArbitrary s (Rep a))
                     =>  CostGen        s      a
genericLessArbitrary = G.to <$> gLessArbitrary
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[2]
instance GLessArbitrary s       f
      => GLessArbitrary s (D1 m f) where 
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
instance GLessArbitrary s G.U1 where
  gLessArbitrary = pure G.U1
  cheapest       = pure G.U1
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[5]
instance (GLessArbitrary  s  a
         ,GLessArbitrary  s          b)
      =>  GLessArbitrary  s (a G.:*: b) where
  gLessArbitrary = (G.:*:) <$> gLessArbitrary
                           <*> gLessArbitrary
  cheapest       = (G.:*:) <$> cheapest
                           <*> cheapest
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[6]
instance  LessArbitrary s         c
      => GLessArbitrary s (G.K1 i c) where
  gLessArbitrary = G.K1 <$> lessArbitrary
  cheapest       = G.K1 <$> lessArbitrary
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|generic-less-arbitrary>>[7]
instance (GLessArbitrary s    a
         ,GLessArbitrary s                  b
         ,KnownNat (SumLen    a)
         ,KnownNat (SumLen                  b)
         ,KnownNat (Cheapness a)
         ,KnownNat (Cheapness               b)
         )
      => GLessArbitrary  s   (a Generic.:+: b) where
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
class StartingState s
   => LessArbitrary s a where
  lessArbitrary :: CostGen s a
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|less-arbitrary-class>>[1]
  default lessArbitrary :: (Generic               a
                           ,GLessArbitrary s (Rep a))
                        =>  CostGen        s      a
  lessArbitrary = genericLessArbitrary
-- ~\~ end

instance StartingState s
      => LessArbitrary s Bool where
  lessArbitrary = flatLessArbitrary

instance StartingState s
      => LessArbitrary s Int where
  lessArbitrary = flatLessArbitrary

instance StartingState s
      => LessArbitrary s Integer where
  lessArbitrary = flatLessArbitrary

instance StartingState s
      => LessArbitrary s Double where
  lessArbitrary = flatLessArbitrary

instance StartingState s
      => LessArbitrary s Char where
  lessArbitrary = flatLessArbitrary

instance (LessArbitrary s  k
         ,LessArbitrary s    v)
      =>  LessArbitrary s (k,v) where

instance (LessArbitrary s          k
         ,Ord                      k)
      =>  LessArbitrary s (Set.Set k) where
  lessArbitrary = Set.fromList <$> lessArbitrary

instance (LessArbitrary s              k
         ,Eq                           k
         ,Ord                          k
         ,Hashable                     k 
         ,LessArbitrary s                v)
      =>  LessArbitrary s (Map.HashMap k v) where
  lessArbitrary =  Map.fromList
               <$> lessArbitrary

instance StartingState s
      => LessArbitrary  s Scientific where
  lessArbitrary =
    scientific <$> lessArbitrary
               <*> lessArbitrary

-- ~\~ begin <<less-arbitrary.md|arbitrary-implementation>>[0]
fasterArbitrary :: forall        s a.
                   LessArbitrary s a
                => QC.Gen          a
fasterArbitrary  = (sizedCost :: CostGen s a -> QC.Gen a) (lessArbitrary :: CostGen s a)

sizedCost :: LessArbitrary s a
          => CostGen       s a
          -> QC.Gen          a
sizedCost gen = QC.sized (`withCost` gen)
-- ~\~ end

flatLessArbitrary :: QC.Arbitrary a
                  => CostGen    s a
flatLessArbitrary  = CostGen $ lift QC.arbitrary

instance LessArbitrary s                a
      => LessArbitrary s (Vector.Vector a) where
  lessArbitrary = Vector.fromList <$> lessArbitrary

-- ~\~ begin <<less-arbitrary.md|lifting-arbitrary>>[0]
instance LessArbitrary s  a
      => LessArbitrary s [a] where
  lessArbitrary = pure [] $$$? do
    budget <- currentBudget
    len  <- choose (1,fromEnum budget)
    spend $ Cost len
    replicateM   len lessArbitrary

instance (QC.Testable            a
         ,LessArbitrary        s a)
      =>  QC.Testable (CostGen s a) where
  property = QC.property
           . sizedCost
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|lifting-arbitrary>>[1]
forAll :: CostGen s a -> (a -> CostGen s b) -> CostGen s b
forAll gen prop = gen >>= prop

oneof   :: HasCallStack
        => [CostGen s a] -> CostGen s a
oneof [] = error
           "LessArbitrary.oneof used with empty list"
oneof gs = choose (0,length gs - 1) >>= (gs !!)

elements :: [a] -> CostGen s a
elements gs = (gs!!) <$> choose (0,length gs - 1)

choose      :: Random  a
            =>        (a, a)
            -> CostGen s a
choose (a,b) = CostGen $ lift $ QC.choose (a, b)

-- | Choose but only up to the budget (for array and list sizes)
budgetChoose :: CostGen s Int
budgetChoose  = do
  Cost b <- currentBudget
  CostGen $ lift $ QC.choose (1, b)

-- | Version of `suchThat` using budget instead of sized generators.
cg `suchThat` pred = do
  result <- cg
  if pred result
     then return result
     else do
       spend 1
       cg `suchThat` pred
-- ~\~ end
-- ~\~ begin <<less-arbitrary.md|lifting-arbitrary>>[2]
frequency   :: HasCallStack
            => [(Int, CostGen s a)]
            ->        CostGen s a
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
