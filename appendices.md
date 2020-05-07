
# Appendix: module headers {.unnumbered}

```{.haskell file=src/Unions.hs .hidden}
{-# language AllowAmbiguousTypes    #-}
{-# language DeriveGeneric          #-}
{-# language DuplicateRecordFields  #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses  #-}
{-# language NamedFieldPuns         #-}
{-# language PartialTypeSignatures  #-}
{-# language ScopedTypeVariables    #-}
{-# language StandaloneDeriving     #-}
{-# language TypeApplications       #-}
{-# language TypeOperators          #-}
{-# language RoleAnnotations        #-}
{-# language TypeSynonymInstances   #-}
{-# language ViewPatterns           #-}
{-# language RecordWildCards        #-}
module Unions where

import           Control.Arrow(second)
import           Data.Aeson
import           Data.Maybe(isJust)
import qualified Data.Foldable as Foldable
import           Data.Function(on)
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding  as Text
import qualified Text.Email.Validate(isValid)
import qualified Data.Set  as Set
import           Data.Set(Set)
import           Data.Scientific
import           Data.List(sortBy)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict(HashMap)
import           GHC.Generics(Generic)
import           Data.Time.ISO8601

<<freetype>>
<<typelike>>
<<basic-constraints>>
<<array-constraint>>
<<object-constraint>>
<<presence-absence-constraints>>
<<union-type-instance>>
<<type>>
<<counted>>

<<missing>>
```

```{.haskell file=test/Spec.hs .hidden}
{-# language FlexibleInstances     #-}
{-# language Rank2Types            #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns        #-}
{-# language ScopedTypeVariables   #-}
{-# language StandaloneDeriving    #-}
{-# language TypeOperators         #-}
{-# language TypeApplications      #-}
{-# language TupleSections         #-}
{-# language UndecidableInstances  #-}
{-# language AllowAmbiguousTypes   #-}
module Main where

import qualified Data.HashMap.Strict as Map
import qualified Data.Set            as Set
import qualified Data.Vector         as Vector
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import Control.Monad(replicateM)
import Data.Scientific
import Data.Aeson
import Data.Proxy
import Data.Typeable
import Data.Hashable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.Validity hiding(check)
import Test.Validity.Monoid
import Test.Validity.Shrinking.Property
import Test.Validity.Utils(nameOf)
import qualified GHC.Generics as Generic

import LessArbitrary
import Unions

instance Arbitrary Value where
  arbitrary = fasterArbitrary

instance LessArbitrary Value where
  lessArbitrary = cheap <$$$?> genericLessArbitrary
    where
      cheap = LessArbitrary.oneof [
                pure       Null
              , Bool   <$> lessArbitrary
              , Number <$> lessArbitrary
              ]

instance Arbitrary Object where
  arbitrary = fasterArbitrary

{-
instance LessArbitrary Object where
  lessArbitrary =
    pure Map.empty <$$$?>
      (Map.fromList <$> lessArbitrary)-}

instance Arbitrary Array where
  arbitrary = fasterArbitrary

class Typelike        ty
   => ArbitraryBeyond ty where
  arbitraryBeyond :: CostGen ty

instance ArbitraryBeyond (PresenceConstraint a) where
  arbitraryBeyond = pure Present

instance ArbitraryBeyond StringConstraint where
  arbitraryBeyond = pure SCAny

instance ArbitraryBeyond IntConstraint where
  arbitraryBeyond = pure IntAny

instance ArbitraryBeyond NumberConstraint where
  arbitraryBeyond = pure NCFloat

instance ArbitraryBeyond RowConstraint where
  arbitraryBeyond = pure RowTop

instance ArbitraryBeyond RecordConstraint where
  arbitraryBeyond = pure RCTop

instance ArbitraryBeyond MappingConstraint where
  arbitraryBeyond = MappingConstraint <$$$> arbitraryBeyond
                                       <*>  arbitraryBeyond

instance (Ord                      a
         ,Show                     a
         )
      => ArbitraryBeyond (FreeType a) where
  arbitraryBeyond = pure Full

instance ArbitraryBeyond ObjectConstraint where
  arbitraryBeyond = do
    ObjectConstraint <$$$> arbitraryBeyond
                      <*>  arbitraryBeyond

instance ArbitraryBeyond ArrayConstraint where
  arbitraryBeyond = do
    ArrayConstraint <$$$> arbitraryBeyond
                     <*>  arbitraryBeyond

instance ArbitraryBeyond UnionType where
  arbitraryBeyond =
    UnionType       <$$$> arbitraryBeyond
                     <*>  arbitraryBeyond
                     <*>  arbitraryBeyond
                     <*>  arbitraryBeyond
                     <*>  arbitraryBeyond
                     <*>  arbitraryBeyond

arbitraryBeyondSpec :: forall          ty.
                       ArbitraryBeyond ty
                    => Spec
arbitraryBeyondSpec =
  prop "arbitraryBeyond returns terms beyond" $
    (beyond <$> (arbitraryBeyond :: CostGen ty))
    
    
{-instance LessArbitrary Array where
  lessArbitrary =
    pure Vector.empty <$$$?>
      (Vector.fromList <$> lessArbitrary)
instance Validity Value where
  validate _ = valid

instance GenUnchecked Text.Text where
  genUnchecked =  Text.pack <$> arbitrary
  shrinkUnchecked = map Text.pack
                  . shrinkUnchecked
                  . Text.unpack

instance GenUnchecked Value where
  genUnchecked = fasterArbitrary

instance GenUnchecked Scientific where
  genUnchecked = scientific <$> arbitrary
                            <*> arbitrary
  shrinkUnchecked _ = []

instance GenUnchecked Object where
  genUnchecked =  Map.fromList <$> fasterArbitrary
  shrinkUnchecked = map Map.fromList
                  . shrinkUnchecked
                  . Map.toList

instance GenUnchecked Array where
  genUnchecked =  Vector.fromList <$> fasterArbitrary
  shrinkUnchecked = map Vector.fromList
                  . shrinkUnchecked
                  . Vector.toList
 -}

instance LessArbitrary Text.Text where
  lessArbitrary = Text.pack <$> lessArbitrary

instance Arbitrary Text.Text where
  arbitrary = Text.pack <$> arbitrary

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary
                         <*> arbitrary

instance (LessArbitrary           a
         ,Ord                     a)
      =>  LessArbitrary (FreeType a) where

instance Arbitrary (FreeType Value) where
  arbitrary = fasterArbitrary
  {-shrink  Full           = []
  shrink (FreeType elts) = map FreeType
                         $ shrink elts-}

instance (Eq                      a
         ,Ord                     a
         ,GenUnchecked            a
         ,LessArbitrary           a
         ,LessArbitrary (FreeType a)
         ,Arbitrary     (FreeType a))
      =>  GenUnchecked  (FreeType a) where
  genUnchecked    = fasterArbitrary
  shrinkUnchecked Full                  = []
  shrinkUnchecked FreeType { captured } =
       map (FreeType . Set.fromList)
     $ shrinkUnchecked
     $ Set.toList captured

instance Validity (FreeType a) where
  validate _ = validate True

instance LessArbitrary (PresenceConstraint a) where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     (PresenceConstraint a) where
  arbitrary = fasterArbitrary

instance LessArbitrary IntConstraint where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     IntConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary NumberConstraint where
  lessArbitrary = genericLessArbitraryMonoid

instance Arbitrary     NumberConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary StringConstraint where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     StringConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary ObjectConstraint where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     ObjectConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary RecordConstraint where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     RecordConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary ArrayConstraint where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     ArrayConstraint where
  arbitrary = fasterArbitrary

{-}
arbitrarySizedArrayConstraint s =
  ArrayConstraint <$> arbitrarySizedConstraint    (s `div` 2)
                  <*> arbitrarySizedRowConstraint (s `div` 2) -}

--arbitrarySizedRowConstraint size = undefined

instance LessArbitrary RowConstraint where
  lessArbitrary = genericLessArbitraryMonoid

instance Arbitrary     RowConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary MappingConstraint where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     MappingConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary UnionType where
  lessArbitrary = genericLessArbitraryMonoid
  {-
    budget <- currentBudget
    if budget > 0
       then id <$$$> (Generic.to <$> lessArbitrary)
       else UnionType <$> lessArbitrary
                      <*> lessArbitrary
                      <*> lessArbitrary
                      <*> lessArbitrary
                      <*> pure mempty -- array
                      <*> pure mempty -- object -}


instance Arbitrary     UnionType where
  arbitrary = fasterArbitrary
  {-sized $ \s ->
    UnionType <$> arbitrary -- NullConstraint
              <*> arbitrary -- BoolConstraint
              <*> arbitrary -- NumberConstraint
              <*> arbitrary -- StringConstraint
              <*> arbitrarySized (s `div` 2) -- ArrayConstraint
              <*> arbitrarySized (s `div` 2)-- ObjectConstraint-}

instance GenUnchecked UnionType where
  genUnchecked    = arbitrary
  shrinkUnchecked = shrink

instance Validity UnionType where
  validate _ = validate True

shrinkSpec :: forall    a.
             (Arbitrary a
             ,Typeable  a
             ,Show      a
             ,Eq        a
             )
           => Spec
shrinkSpec = prop ("shrink on " <> nameOf @a)
           $ doesNotShrinkToItself arbitrary (shrink :: a -> [a])

allSpec :: forall         ty v.
           (Typeable        ty
            ,Arbitrary       ty
            ,Show            ty
            ,Types           ty v
            ,ArbitraryBeyond ty
            ,Arbitrary          v
            ,Show               v
            ) => Spec
allSpec = describe (nameOf @ty) $ do
  arbitraryBeyondSpec @ty
  shrinkSpec    @ty
  --typelikeSpec  @ty
  --typesSpec     @ty @v

spec = do
  describe "Value" $ do
    --arbitrarySpec @Value
    shrinkSpec    @Value
    --describe "Free types" $ do
  allSpec @(FreeType Value) @Value
    {-arbitrarySpec @(FreeType Value)
    shrinkSpec    @(FreeType Value)
    typelikeSpec  @(FreeType Value)
    typesSpec     @(FreeType Value) @Value-}
  allSpec @NullConstraint    @()
  allSpec @BoolConstraint    @Bool
  allSpec @StringConstraint  @Text.Text
  allSpec @IntConstraint     @Int
  allSpec @NumberConstraint  @Scientific
  {-allSpec @RowConstraint     @Array
  allSpec @ArrayConstraint   @Array
  allSpec @MappingConstraint @Object
  allSpec @RecordConstraint  @Object
  allSpec @ObjectConstraint  @Object
  allSpec @UnionType         @Value-}

<<typelike-spec>>
<<types-spec>>

return []
main = $quickcheckAll
--hspec spec
```

# Appendix: package dependencies {.unnumbered}

```{.yaml .hpack file=package.yaml}
name: union-types
version: '0.1.0.0'
category: Web
author: Anonymous
maintainer: example@example.com
license: BSD-3
extra-source-files:
- CHANGELOG.md
- README.md
dependencies:
- base
- aeson
- containers
- text
- hspec
- QuickCheck
- unordered-containers
- scientific
- hspec
- QuickCheck
- validity
- vector
- unordered-containers
- scientific
- genvalidity
- genvalidity-hspec
- genvalidity-property
- iso8601-time
- time
- email-validate
- generic-arbitrary
- mtl
- hashable
library:
  source-dirs: src
  exposed-modules:
  - Unions
tests:
  spec:
    main: Spec.hs
    source-dirs: test
    dependencies:
      - union-types
      - mtl
      - random
      - transformers
      - hashable
```

# Appendix: Hindley-Milner as `Typelike` {.unnumbered}

# Appendix: Missing pieces of code {.unnumbered}

``` {.haskell #missing .hidden}
-- In order to represent `FreeType` for the `Value`,
-- we need to add `Ord` instance for it:
instance Ord       Value where
  compare = compare `on` encodeConstructors

fromEnum' :: Enum a => a -> Integer
fromEnum' = fromIntegral . fromEnum

encodeConstructors :: Value -> [Integer]
encodeConstructors  Null      = [0]
encodeConstructors (Bool   b) = [1, fromEnum' b]
encodeConstructors (Number n) = [2,
        fromIntegral $ base10Exponent n,
        coefficient n]
encodeConstructors (String s) = 3:
  (fromEnum' <$> Text.unpack s)
encodeConstructors (Array  a) = 4:
  concatMap encodeConstructors a
encodeConstructors (Object o) =
    concatMap encodeItem      $
    sortBy (compare `on` fst) $
    Map.toList o
  where
    encodeItem (k, v) =
      (fromEnum' <$> Text.unpack k) <>
      encodeConstructors v
```

```{.haskell #missing}

isValidDate :: Text -> Bool
isValidDate = isJust
            . parseISO8601
            . Text.unpack

isValidEmail :: Text -> Bool
isValidEmail = Text.Email.Validate.isValid
           . Text.encodeUtf8
```

# Appendix: Less arbitrary wait time


```{.haskell file=test/LessArbitrary.hs}
{-# language DefaultSignatures     #-}
{-# language FlexibleInstances     #-}
{-# language FlexibleContexts      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language Rank2Types            #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables   #-}
{-# language StandaloneDeriving    #-}
{-# language TypeOperators         #-}
{-# language TypeFamilies          #-}
{-# language TypeApplications      #-}
{-# language TupleSections         #-}
{-# language UndecidableInstances  #-}
{-# language AllowAmbiguousTypes   #-}
{-# language DataKinds             #-}
{-# language KindSignatures        #-}
module LessArbitrary(
    LessArbitrary(..)
  , oneof
  , CostGen(..)
  , (<$$$>)
  , (<$$$?>)
  , currentBudget
  , fasterArbitrary
  , genericLessArbitrary
  , genericLessArbitraryMonoid
  , flatLessArbitrary
  , spend
  ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Set            as Set
import qualified Data.Vector         as Vector
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import Control.Monad(replicateM)
import Data.Scientific
import Data.Aeson
import Data.Proxy
import Data.Typeable
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QC
import Test.Hspec
import Test.Hspec.QuickCheck
--import Test.QuickCheck.Arbitrary.Generic
import Test.Validity hiding(check)
import Test.Validity.Monoid
import Test.Validity.Shrinking
import Test.Validity.Utils(nameOf)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Trans.Class
import System.Random(Random)
import Control.Applicative
import Data.Proxy
import GHC.Generics as G
import GHC.TypeLits
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Arbitrary as QC
import Data.Hashable

newtype Cost = Cost { unCost :: Int }
  deriving (Eq,Ord,Enum,Bounded,Num)

newtype CostGen                               a =
        CostGen {
          runCostGen :: State.StateT Cost QC.Gen a }
  deriving (Functor, Applicative, Monad, State.MonadFix)

-- Mark a costly constructor with this instead of `<$>`
(<$$$>) :: (a -> b) -> CostGen a -> CostGen b
costlyConstructor <$$$> arg = do
  spend 1
  costlyConstructor <$> arg

spend :: Cost -> CostGen ()
spend c = CostGen $ State.modify (-c+)

oneof   :: [CostGen a] -> CostGen a
oneof [] = error "LessArbitrary.oneof used with empty list"
oneof gs = choose (0,length gs - 1) >>= (gs !!)

elements :: [a] -> CostGen a
elements gs = (gs!!) <$> choose (0,length gs - 1)

choose      :: Random  a
            =>        (a, a)
            -> CostGen a
choose (a,b) = CostGen $ lift $ QC.choose (a, b)

(<$$$?>) :: CostGen a -> CostGen a -> CostGen a
cheapVariants <$$$?> costlyVariants = do
  budget <- CostGen State.get
  if budget > (0 :: Cost)
     then costlyVariants
     else cheapVariants

currentBudget :: CostGen Cost
currentBudget = CostGen State.get

-- | Chooses one of the given generators, with a weighted random distribution.
-- The input list must be non-empty.
frequency :: [(Int, CostGen a)] -> CostGen a
frequency [] = error "LessArbitrary.frequency used with empty list"
frequency xs
  | any (< 0) (map fst xs) =
    error "LessArbitrary.frequency: negative weight"
  | all (== 0) (map fst xs) =
    error "LessArbitrary.frequency: all weights were zero"
frequency xs0 = choose (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "LessArbitrary.pick used with empty list"

costFrequency :: [(Int, CostGen a)] -> CostGen a
costFrequency [] = error "LessArbitrary.costFrequency used with empty list"
costFrequency xs = do
  budget <- currentBudget
  if budget>0
    then frequency                  xs
    else frequency
       $ filter ((minFreq==) . fst) xs
  where
    minFreq = minimum $ map fst xs


withCost :: Int -> CostGen a -> QC.Gen a
withCost cost gen = runCostGen gen
  `State.evalStateT` Cost cost

defaultCost :: Int
defaultCost  = 100

{-
instance LessArbitrary a
      => QC.Arbitrary  a where
  arbitrary = withCost defaultCost $ lessArbitrary
  -}

class LessArbitrary a where
  lessArbitrary :: CostGen a
  default lessArbitrary :: (Generic a, CGArbitrary (Rep a)) => CostGen a
  lessArbitrary = to <$> cgArbitrary
  
class CGArbitrary a where
  cgArbitrary :: CostGen (a x)

instance CGArbitrary G.U1 where
  cgArbitrary = pure G.U1

instance LessArbitrary       c
      => CGArbitrary (G.K1 i c) where
  cgArbitrary = G.K1 <$> lessArbitrary

instance CGArbitrary f
      => CGArbitrary (G.M1 i c f) where
  cgArbitrary = G.M1 <$> cgArbitrary

instance (CGArbitrary a, CGArbitrary b) => CGArbitrary (a G.:*: b) where
  cgArbitrary = liftA2 (G.:*:) cgArbitrary cgArbitrary

-- | Calculates count of constructors encoded by particular ':+:'.
-- Internal use only.
type family SumLen a :: Nat where
  SumLen (a G.:+: b) = (SumLen a) + (SumLen b)
  --SumLen (a G.:*: b) = (SumLen a) + (SumLen b)
  SumLen  a          =  1

type family ConsCost a :: Nat where
  ConsCost (a G.:*: b) =     (ConsCost a) + (ConsCost b)
  ConsCost (a G.:+: b) = Min (ConsCost a) (ConsCost b)
  ConsCost  a          = 1

instance (CGArbitrary      a,  CGArbitrary      b,
          KnownNat (SumLen a), KnownNat (SumLen b)
         ) => CGArbitrary (a G.:+:              b) where
  cgArbitrary = costFrequency
    [ (lfreq, G.L1 <$$$> cgArbitrary)
    , (rfreq, G.R1 <$$$> cgArbitrary) ]
    where
      lfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen a))
      rfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen b))

fasterArbitrary :: LessArbitrary a => QC.Gen a
fasterArbitrary = sizedCost lessArbitrary

genericLessArbitrary :: (Generic a, CGArbitrary (Rep a)) => CostGen a
genericLessArbitrary = to <$> cgArbitrary

genericLessArbitraryMonoid :: (Generic          a
                              ,CGArbitrary (Rep a)
                              ,Monoid           a)
                           =>  CostGen          a
genericLessArbitraryMonoid  =
  pure mempty <$$$?> genericLessArbitrary

type family Min m n where
  Min m n = Min_ m n (CmpNat m n)

type family Min_ (m::Nat) (n::Nat) (o::Ordering) where 
  Min_ m n 'LT = m
  Min_ m n 'EQ = m
  Min_ m n 'GT = n

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

instance LessArbitrary  a
      => LessArbitrary [a] where
  lessArbitrary = pure [] <$$$?> do
    len  <- choose (1,100) -- FIXME: use sized
    spend $ Cost len
    replicateM   len lessArbitrary

instance LessArbitrary Scientific where
  lessArbitrary =
    scientific <$> lessArbitrary
               <*> lessArbitrary

flatLessArbitrary :: QC.Arbitrary a
              => CostGen a
flatLessArbitrary  = CostGen $ lift QC.arbitrary

instance LessArbitrary                a
      => LessArbitrary (Vector.Vector a) where
  lessArbitrary = Vector.fromList <$> lessArbitrary

sizedCost :: CostGen a -> QC.Gen a
sizedCost gen = QC.sized $ (`withCost` gen)

instance QC.Testable          a
      => QC.Testable (CostGen a) where
  property = QC.property
           . sizedCost

```
