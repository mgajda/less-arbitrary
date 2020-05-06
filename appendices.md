
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

import           Control.Arrow(second, (***), first)
import           Data.Aeson
import           Data.Maybe(isJust)
import qualified Data.Foldable as Foldable
import           Data.Time.Calendar(Day)
import           Data.Either
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
import           Data.Time.Clock(UTCTime(utctDay))


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

import Unions

-- FIXME: use json-autotype's
instance Arbitrary Value where
  arbitrary = sized arbitraryValue
  shrink _ = []

instance Validity Value where
  validate _ = valid

instance GenUnchecked Value where
  genUnchecked    = sized arbitraryValue
  shrinkUnchecked  Null      = []
  shrinkUnchecked (Bool   _) = []  
  shrinkUnchecked (String _) = []  
  shrinkUnchecked (Number _) = []
  shrinkUnchecked (Array  a) =
    Array . Vector.fromList <$>
      shrinkList shrink (Vector.toList a)
  shrinkUnchecked (Object o) =
    Object . Map.fromList <$> shrinkList shrinkItemValue (Map.toList o)

shrinkItemValue (k, v) =
  (k,) <$> shrink v

arbitraryValue 0    = oneof [
             pure Null
           , Bool   <$> arbitrary
           , String <$> arbitrary
           , Number <$> arbitrary
           ]
arbitraryValue size = oneof [
             pure Null
           , Bool   <$> arbitrary
           , String <$> arbitrary
           , Number <$> arbitrary
           , Array   .  Vector.fromList
                    <$> listOf recurse
           -- FIXME
           {-, Object <$> Map.fromList
                    <$> listSized (\s -> (,) <$> arbitrary
                                             <*> arbitraryValue s)-}
           ]
  where
    listSized gen = sized $ \size -> do
      listSize <- choose (0,size)
      replicateM           listSize
               $ gen (size `div` listSize)
    recurse = arbitraryValue
            $ size `div` 8

instance Arbitrary Text.Text where
  arbitrary = Text.pack <$> arbitrary

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary
                         <*> arbitrary

instance Arbitrary (FreeType Value) where
  arbitrary = sized $ \size -> do
    r <-choose (0,10::Int)
    if r == 0
      then pure Full
      else FreeType . Set.fromList <$>
             listOf (arbitraryValue
                       (size `div` 10))
  shrink  Full           = []
  shrink (FreeType elts) = map FreeType
                         $ shrink elts

instance (Arbitrary    (FreeType a))
      =>  GenUnchecked (FreeType a) where
  genUnchecked    = arbitrary
  shrinkUnchecked = shrink

instance Validity (FreeType a) where
  validate _ = validate True

instance Arbitrary (PresenceConstraint a) where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary IntConstraint where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary NumberConstraint where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary StringConstraint where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary ObjectConstraint where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary RecordConstraint where
  arbitrary = RecordConstraint . Map.fromList
           <$> arbitrary
  shrink (RecordConstraint map) = do
    RecordConstraint . Map.fromList <$> shrink (Map.toList map)

instance Arbitrary ArrayConstraint where
  arbitrary = genericArbitrary
  shrink    = genericShrink

arbitrarySizedArrayConstraint s =
  ArrayConstraint <$> arbitrarySizedArrayConstraint (s `div` 2)
                  <*> arbitrarySizedRowConstraint   (s `div` 2)

arbitrarySizedRowConstraint size = undefined

instance Arbitrary  RowConstraint where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary MappingConstraint where
  arbitrary = genericArbitrary
  shrink    = genericShrink

instance Arbitrary UnionType where
  arbitrary = sized $ \s ->
    UnionType <$> arbitrary -- NullConstraint
              <*> arbitrary -- BoolConstraint
              <*> arbitrary -- NumberConstraint
              <*> arbitrary -- StringConstraint
              <*> arbitrarySized (s `div` 2) -- ArrayConstraint
              <*> arbitrarySized (s `div` 2)-- ObjectConstraint
  shrink    = genericShrink

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
