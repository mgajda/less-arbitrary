
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
import Test.Validity.Shrinking
import Test.Validity.Utils(nameOf)

import LessArbitrary
import Unions

instance Arbitrary Value where
  arbitrary = genericLessArbitrary

instance LessArbitrary Value where

instance Validity Value where
  validate _ = valid

instance GenUnchecked Text.Text where
  genUnchecked =  Text.pack <$> genericLessArbitrary
  shrinkUnchecked = map Text.pack
                  . shrinkUnchecked
                  . Text.unpack

instance GenUnchecked Value where
  genUnchecked = genericLessArbitrary

instance GenUnchecked Scientific where
  genUnchecked = scientific <$> arbitrary <*> arbitrary
  shrinkUnchecked _ = []

instance GenUnchecked Object where
  genUnchecked =  Map.fromList <$> genericLessArbitrary
  shrinkUnchecked = map Map.fromList
                  . shrinkUnchecked
                  . Map.toList

instance GenUnchecked Array where
  genUnchecked =  Vector.fromList <$> genericLessArbitrary
  shrinkUnchecked = map Vector.fromList
                  . shrinkUnchecked
                  . Vector.toList

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
  arbitrary = genericLessArbitrary
  shrink  Full           = []
  shrink (FreeType elts) = map FreeType
                         $ shrink elts

instance (Eq                      a
         ,Ord                     a
         ,GenUnchecked            a
         ,LessArbitrary           a
         ,LessArbitrary (FreeType a)
         ,Arbitrary     (FreeType a))
      =>  GenUnchecked  (FreeType a) where
  genUnchecked    = genericLessArbitrary
  shrinkUnchecked Full                  = []
  shrinkUnchecked FreeType { captured } =
       map (FreeType . Set.fromList)
     $ shrinkUnchecked
     $ Set.toList captured

instance Validity (FreeType a) where
  validate _ = validate True

instance LessArbitrary (PresenceConstraint a) where
instance Arbitrary     (PresenceConstraint a) where
  arbitrary = genericLessArbitrary

instance LessArbitrary IntConstraint where
instance Arbitrary     IntConstraint where
  arbitrary = genericLessArbitrary

instance LessArbitrary NumberConstraint where
instance Arbitrary     NumberConstraint where
  arbitrary = genericLessArbitrary

instance LessArbitrary StringConstraint where
instance Arbitrary     StringConstraint where
  arbitrary = genericLessArbitrary

instance LessArbitrary ObjectConstraint where
instance Arbitrary     ObjectConstraint where
  arbitrary = genericLessArbitrary

instance LessArbitrary RecordConstraint where
instance Arbitrary     RecordConstraint where
  arbitrary = genericLessArbitrary

instance LessArbitrary ArrayConstraint where
instance Arbitrary     ArrayConstraint where
  arbitrary = genericLessArbitrary

{-}
arbitrarySizedArrayConstraint s =
  ArrayConstraint <$> arbitrarySizedConstraint    (s `div` 2)
                  <*> arbitrarySizedRowConstraint (s `div` 2) -}

--arbitrarySizedRowConstraint size = undefined

instance LessArbitrary RowConstraint where
instance Arbitrary     RowConstraint where
  arbitrary = genericLessArbitrary

instance LessArbitrary MappingConstraint where
instance Arbitrary     MappingConstraint where
  arbitrary = genericLessArbitrary

instance LessArbitrary UnionType where
instance Arbitrary     UnionType where
  arbitrary = genericLessArbitrary
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

main = hspec spec

spec = do
  describe "Value" $ do
    arbitrarySpec @Value
    prop "shrink" $ shrinkUncheckedDoesNotShrinkToItself @Value
  describe "Free types" $ do
    arbitrarySpec @(FreeType Value)
    prop "shrink" $ shrinkUncheckedDoesNotShrinkToItself @(FreeType Value)
    typelikeSpec  @(FreeType Value)
    typesSpec     @(FreeType Value) @Value
  describe "JSON types" $ do
    arbitrarySpec @UnionType
    prop "shrink" $ shrinkUncheckedDoesNotShrinkToItself @UnionType
    typelikeSpec  @UnionType
    typesSpec     @UnionType @Value

<<typelike-spec>>
<<types-spec>>
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
  , (<$$$>)
  , (<$$$?>)
  , currentBudget
  , genericLessArbitrary
  , flatArbitrary
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

newtype Cost = Cost Int
  deriving (Eq,Ord,Enum,Bounded,Num)

newtype CostGen                               a =
        CostGen {
          runCostGen :: State.StateT Cost QC.Gen a }
  deriving (Functor, Applicative, Monad, State.MonadFix)

-- Mark a costly constructor with this instead of `<$>`
(<$$$>) :: (a -> b) -> CostGen a -> CostGen b
costlyConstructor <$$$> arg = do
  CostGen $ State.modify (-1+)
  costlyConstructor <$> arg

oneof   :: [CostGen a] -> CostGen a
oneof [] = error "LessArbitrary.oneof used with empty list"
oneof gs = choose (0,length gs - 1) >>= (gs !!)

elements :: [a] -> CostGen a
elements gs = (gs!!) <$> choose (0,length gs - 1)

choose      :: Random  a
            =>        (a, a)
            -> CostGen a
choose (a,b) = CostGen $ lift $ QC.choose (a, b)

(<$$$?>) :: [CostGen a] -> [CostGen a] -> CostGen a
cheapVariants <$$$?> costlyVariants = do
  budget <- CostGen State.get
  oneof $ cheapVariants
       <> if budget > (0 :: Cost)
             then costlyVariants
             else [] 

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
defaultCost = 1000

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

instance CGArbitrary f => CGArbitrary (G.M1 i c f) where
  cgArbitrary = G.M1 <$> cgArbitrary

instance (CGArbitrary a, CGArbitrary b) => CGArbitrary (a G.:*: b) where
  cgArbitrary = liftA2 (G.:*:) cgArbitrary cgArbitrary

-- | Calculates count of constructors encoded by particular ':+:'.
-- Internal use only.
type family SumLen a :: Nat where
  SumLen (a G.:+: b) = (SumLen a) + (SumLen b)
  SumLen (a G.:*: b) = (SumLen a) + (SumLen b)
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

genericLessArbitrary :: (Generic a, CGArbitrary (Rep a)) => QC.Gen a
genericLessArbitrary = withCost defaultCost (to <$> cgArbitrary)

type family Min m n where
  Min m n = Min_ m n (CmpNat m n)

type family Min_ (m::Nat) (n::Nat) (o::Ordering) where 
  Min_ m n 'LT = m
  Min_ m n 'EQ = m
  Min_ m n 'GT = n

instance LessArbitrary Bool where
  lessArbitrary = flatArbitrary

instance LessArbitrary Int where
  lessArbitrary = flatArbitrary

instance LessArbitrary Integer where
  lessArbitrary = flatArbitrary

instance LessArbitrary Double where
  lessArbitrary = flatArbitrary

instance LessArbitrary Char where
  lessArbitrary = flatArbitrary

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

instance LessArbitrary Scientific where
  lessArbitrary =
    scientific <$> lessArbitrary
               <*> lessArbitrary

flatArbitrary :: QC.Arbitrary a
              => CostGen a
flatArbitrary  = CostGen $ lift QC.arbitrary

instance LessArbitrary                a
      => LessArbitrary (Vector.Vector a) where
  lessArbitrary = Vector.fromList <$> lessArbitrary

```
