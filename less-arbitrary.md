---
title:  "Less Arbitrary waiting time"
shorttitle: "Less Arbitrary"
author:
  - name: Michał J. Gajda
    email: mjgajda@migamake.com
    affiliation:
      institution: Migamake Pte Ltd
tags:
  - Haskell
  - property testing
  - QuickCheck
abstract: |
  Property testing is the cheapest and most precise way of building up a test suite for your program.
  Especially if the datatypes enjoy nice mathematical laws.
  But it is also the easiest way to make it run for an unreasonably long time.
  We prove connection between deeply recursive data structures, and epidemic growth rate, and show how to fix the problem, and make Arbitrary instances run in linear time with respect to assumed test size.
date:   2020-05-05
description: |
link: "https://gitlab.com/migamake/less-arbitrary"
bibliography:
  - towards-better-union.bib
csl: acm-template/acm-sig-proceedings.csl
prologue: |
  \let\longtable\tabular
  \let\endlongtable\endtabular
  \renewcommand{\url}[1]{\href{#1}{[Link]}}
link-citations: true
tables: true
listings: true
acks: |

  The text was prepared with great help of bidirectional literate programming[@literate-programming] tool[@entangled],
  Pandoc[@pandoc] markdown publishing system and live feedback from Stack file watching option[@haskell-stack].

---

# Introduction

Property testing is the cheapest and most precise way of building up a test suite for your program.
Especially if the datatypes enjoy nice mathematical laws.
But it is also the easiest way to make it run for an unreasonably long time.
We prove connection between deeply recursive data structures, and epidemic growth rate, and show how to fix the problem.
After our intervention the Arbitrary instances run in linear time with respect to assumed test size.

# Motivation

Typical arbitrary instance just draws
a random constructor from a set,
possibly biasing certain outcomes.

**Generic** arbitrary instance looks like this:

```{.haskell #tree-type}
data Tree        a =
    Leaf         a
  | Branch [Tree a]
  deriving (Eq,Show,Generic.Generic)

instance Arbitrary       a
      => Arbitrary (Tree a) where
  arbitrary = oneof [Leaf   <$> arbitrary
                    ,Branch <$> arbitrary
                    ]
```

Assuming we run QuickCheck with any size parameter greater than 1,
it will fail to terminate!

List instances is a wee bit better,
since it tries to limit maximum list length to a constant
option:

```{.haskell}
instance Arbitrary  a
      => Arbitrary [a] where
  lessArbitrary = sized $ \size do
    len  <- choose (1,size)
    vectorOf len lessArbitrary
```

Indeed QuickCheck manual[@quickcheck-manual]^[We changed `liftM` and `liftM2` to operators for consistency.],
suggests an error-prone manual method of limiting
the depth of generated structure:
```{.haskell}
data Tree = Leaf Int | Branch Tree Tree

instance Arbitrary Tree where
  arbitrary = sized tree'
    where tree' 0 = Leaf <$> arbitrary
	  tree' n | n>0 = 
		oneof [Leaf   <$> arbitrary,
   	               Branch <$> subtree <*> subtree]
  	    where subtree = tree' (n `div` 2)
```

This fixes non-termination issue, but still may lead to unpredictable waiting times for nested structures.
The depth of the generated structure is linearly limited
by dividing the `n` by expected branching factor of the recursive data structure.
However this does not work very well for mutually recursive data structures
occuring in compilers[@compilersALaCarte],
which may have 30 constructors with variable branching factor
just like GHC's `HSExpr` data types.

Here using `div`ision of size by maximum branching factor
will decrease coverage into relatively deep data structures,
whereas `div`iding by average branching factor of `~2` will
generate both deep and very large structures.

Now we have a choice of manual generation of these data structures,
which certainly introduces bias in testing, or abandoning property testing
for real-life-sized projects.

# Complexity analysis

We might be tempted to compute average size of the structure.
Let's use viral reproduction rate estimate for a single
rewrite of `arbitrary` function written in conventional way.

We compute a number of recursive references for each constructor.
Then we take an average number of references among all the constructors.
If it is greater than 1, any **non-lazy** property test will certainly fail to terminate.
If it is slightly smaller, we still can wait a long time.

What is an issue here is not just non-termination which is fixed by error-prone manual process
of writing own instances that use explicit `size` parameter.

The much worse issue is unpredictability of the test runtime.

Given a _maximum size_ parameter (as it is now called) to QuickCheck,
would we not expect that tests terminate within linear time of this parameter?
At least if our computation algorithms are linear with respect to input size?

Currently for any recursive structure like `Tree a`,
we see some exponential function. For example $ size^n $, where $n$ is a random variable.

# Solution

We propose to replace implementation with a simple state monad that actually
remembers how many constructors were generated,
and thus avoid limiting the depth of generated data structures,
and ignoring estimation of branching factor altogether.

```{.haskell #costgen}
newtype Cost = Cost { unCost :: Int }
  deriving (Eq,Ord,Enum,Bounded,Num)

newtype CostGen                               a =
        CostGen {
          runCostGen :: State.StateT Cost QC.Gen a }
  deriving (Functor, Applicative, Monad, State.MonadFix)
```

We track the spending in the usual way:
```{.haskell #spend}
spend :: Cost -> CostGen ()
spend c = CostGen $ State.modify (-c+)
````

Then we limit our choices when budget is tight:
```{.haskell #budget}
currentBudget :: CostGen Cost
currentBudget = CostGen State.get
```

To make generation easier, we introduce `budget check` operator:
```{.haskell #budget}
($$$?) :: CostGen a -> CostGen a -> CostGen a
cheapVariants $$$? costlyVariants = do
  budget <- CostGen State.get
  if budget > (0 :: Cost)
     then costlyVariants
     else cheapVariants
```

```{.haskell #budget .hidden}
(<$$$?>) = ($$$?)
```

In order to conveniently define our budget generators,
we might want to define a class for them:

```{.haskell #less-arbitrary-class}
class LessArbitrary a where
  lessArbitrary :: CostGen a
```

```{.haskell #less-arbitrary-class .hidden}
  default lessArbitrary :: (Generic             a
                           ,GLessArbitrary (Rep a))
                        =>  CostGen             a
  lessArbitrary = genericLessArbitrary
```

Then we can use them as implementation of `arbitrary`
that should have been always used:

```{.haskell #arbitrary-implementation}
fasterArbitrary :: LessArbitrary a => QC.Gen a
fasterArbitrary = sizedCost lessArbitrary

sizedCost :: CostGen a -> QC.Gen a
sizedCost gen = QC.sized $ (`withCost` gen)
```

Then we can implement `Arbitrary` instances simply with:

```{.haskell}
instance _
      => Arbitrary a where
  arbitrary = fasterArbitrary
```

Of course we still need to define `LessArbitrary`,
but after seeing how simple was a `Generic` defintion `Arbitrary`
we have a hope that our implementation will be:


```{.haskell}
instance LessArbitrary where
```
That is - we hope that the the generic implementation will take over.

# Introduction to GHC generics

Generics allow us to provide default instance,
by encoding any datatype into its generic `Rep`resentation:
```{.haskell #generic-instance}
instance Generics (Tree a) where
  to :: Tree a -> Rep (Tree a)
  from :: Rep (Tree a) -> Tree a
```

The secret to making a generic function
is to create a set of `instance` declarations
for each type family constructor.

So let's examine `Rep`resentation of our working example,
and see how to declare instances:

1. First we see datatype metadata `D1` that shows where our type was defined:
```{.haskell #generic-instance-other}
type instance Rep (Tree a) =
  D1
   ('MetaData "Tree" "Test.Arbitrary" "less-arbitrary" 'False)
```

2. Then we have constructor metadata `C1`:
```{.haskell}
       (C1
          ('MetaCons "Leaf" 'PrefixI 'False)
```
3. Then we have metadata for each field selector within a constructor:
```{.haskell}
          (S1
             ('MetaSel
                'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
```
4. And reference to another datatype in the record field value:
```{.haskell}
             (Rec0 a))
```
5. Different constructors are joined by sum type operator:
```{.haskell}
        :+:
```
6. Second constructor has a similar representation:
```{.haskell}
        C1
              ('MetaCons "Branch" 'PrefixI 'False)
              (S1
                 ('MetaSel
                    'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                    (Rec0 [Tree a])))
                     ignored
```
7. Note that `Rep`resentation type constructors have additional parameter that is not relevant for our use case.

For simple datatypes, we are only interested in three constructors:

* `:+:` encode choice between constructors
* `:*:` encode a sequence of constructor parameters
* `M1`  encode metainformation about the named constructors, `C1`, `S1` and `D1` are actually
        shorthands for `M1 C`, `M1 S` and `M1 D`

There are more short cuts to consider:
* `U1`  is the unit type (no fields)
* `Rec0` is another type in the field

## Example of generics

This generic representation can then be matched by generic instances.
Example of `Arbitrary` instance from [@generic-arbitrary] serves as a basic example^[We modified class name to simplify.]

1. First we convert the type to its generic representation:

```{.haskell}
genericArbitrary :: (Generic a, Arbitrary (Rep a)) => Gen a
genericArbitrary = to <$> arbitrary
```

2. We take care of nullary constructors with:

```{.haskell}
instance Arbitrary G.U1 where
  arbitrary = pure G.U1
```
3. For all fields arguments are recursively calling `Arbitrary` class method:

```{.haskell}
instance Arbitrary c => Arbitrary (G.K1 i c) where
  gArbitrary = G.K1 <$> arbitrary
```
4. We skip metadata by the same recursive call:

```{.haskell}
instance Arbitrary           f
      => Arbitrary (G.M1 i c f) where
  arbitrary = G.M1 <$> arbitrary
```

5. Given that all arguments of each constructor are joined by `:*:`,
we need to recursively delve there too:

```{.haskell }
instance (Arbitrary  a,
         ,Arbitrary          b)
      =>  Arbitrary (a G.:*: b) where
  arbitrary = (G.:*:) <$> arbitrary <*> arbitrary
```

6. In order to sample all constructors with the same probability we compute
a number of constructor in each representation type with `SumLen` type family:

```{.haskell}
-- | Calculates count of constructors encoded by particular ':+:'.
-- Internal use only.
type family SumLen a :: Nat where
  SumLen (a G.:+: b) = (SumLen a) + (SumLen b)
  SumLen a           = 1
```

Now that we have number of constructors computed, we can draw them with equal probability:
```{.haskell}
instance (Arbitrary        a
         ,Arbitrary                b
         ,KnownNat (SumLen a)
         ,KnownNat (SumLen         b)
         )
      =>  Arbitrary       (a G.:+: b) where
  arbitrary = frequency
    [ (lfreq, G.L1 <$> arbitrary)
    , (rfreq, G.R1 <$> arbitrary) ]
    where
      lfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen a))
      rfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen b))
```

Excellent piece of work, but non-terminating for recursive types
with average branching factor greater than 1 (and non-lazy tests, like checking `Eq` reflexivity.)

## Implementing with Generics

It is apparent from our previous considerations,
that we can reuse code from the existing generic
implementation when the budget is positive.
We just need to spend a dollar for
each constructor we encounter.

For the `Monoid` the implementation would be trivial:
```{.haskell #generic-less-arbitrary}
genericLessArbitraryMonoid :: (Generic             a
                              ,GLessArbitrary (Rep a)
                              ,Monoid              a )
                           =>  CostGen             a
genericLessArbitraryMonoid  =
  pure mempty <$$$?> genericLessArbitrary
```

However we want to have fully generic implementation
that chooses the cheapest constructor even though the
datatype does not have monoid instance.

### Helpful type family

First we need to compute minimum cost of the in each
branch of the type representation.
Instead of calling it _minimum cost_, we call this function `Cheapness`.

For this we need to implement minimum function at the type level:

```{.haskell #generic-instances}
type family Min m n where
  Min m n = ChooseGreater (CmpNat m n) m n

type family ChooseGreater (o::Ordering) (m::Nat) (n::Nat) where 
  ChooseGreater 'LT m n = m
  ChooseGreater 'EQ m n = m
  ChooseGreater 'GT m n = n
```

so we can choose the cheapest^[We could add instances for :
```{.haskell #generic-instances}
type family Cheapness a :: Nat where
  Cheapness (a :*: b) =      Cheapness a + Cheapness b
  Cheapness (a :+: b) = Min (Cheapness a) (Cheapness b)
  Cheapness (S1 a  b) = 1
  <<flat-types>>
```

Since we are only interested in recursive types that can potentially blow out
our budget, we can also add cases for flat types since they seem the cheapest:

```{.haskell #flat-types}
Cheapness (S1 a (Rec0 Int       )) = 0
Cheapness (S1 a (Rec0 Scientific)) = 0
Cheapness (S1 a (Rec0 Double    )) = 0
Cheapness (S1 a (Rec0 Bool      )) = 0
Cheapness (S1 a (Rec0 Text.Text )) = 0
```

### Skipping over metadata

First we safely ignore metadata by writing an instance:

```{.haskell #generic-instances}
instance GLessArbitrary           f
      => GLessArbitrary (G.M1 i c f) where
  gLessArbitrary = G.M1 <$> gLessArbitrary
```

### Generation on budget

When the budget is low,
we need to find the least costly constructor each time.

We implement it as a type class `GLessArbitrary` that
is implemented for parts of the `Generic` `Rep`resentation type:

```{.haskell #generic-less-arbitrary}
class GLessArbitrary datatype where
  gLessArbitrary :: CostGen (datatype p)

genericLessArbitrary :: (Generic             a
                        ,GLessArbitrary (Rep a))
                     =>  CostGen             a
genericLessArbitrary = to <$> gLessArbitrary
```

We need to use a type family that computes a cost of a generic implementation:

```{.haskell #generic-less-arbitrary}
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
``` 

Naturally, `GLessArbitrary` for unit type has only one result:

```{.haskell #generic-less-arbitrary}
instance GLessArbitrary G.U1 where
  gLessArbitrary = pure G.U1
```

We descend down the product of to reach each field,
and then assemble the result:

```{.haskell #generic-less-arbitrary}
instance (GLessArbitrary  a
         ,GLessArbitrary          b)
      =>  GLessArbitrary (a G.:*: b) where
  gLessArbitrary = (G.:*:) <$> gLessArbitrary <*> gLessArbitrary
```

We recursively call instances of `LessArbitrary` for the types of fields:

```{.haskell #generic-less-arbitrary}
instance  LessArbitrary         c
      => GLessArbitrary (G.K1 i c) where
  gLessArbitrary = G.K1 <$> lessArbitrary
```

But we will need to make a choice on which constructor to use:

```{.haskell #generic-less-arbitrary}
instance (GLessArbitrary   a
         ,GLessArbitrary                 b
         ,KnownNat (SumLen a)
         ,KnownNat (SumLen               b)
         )
      => GLessArbitrary   (a Generic.:+: b) where
  gLessArbitrary = do
    spend 1
    costFrequency
      [ (lfreq, L1 <$> gLessArbitrary)
      , (rfreq, R1 <$> gLessArbitrary) ]
    where
      lfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen a))
      rfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen b))
```


# Bibliography

::::: {#refs}

:::::

# Appendix: Module headers

```{.haskell file=test/lib/Test/LessArbitrary.hs}
{-# language DefaultSignatures     #-}
{-# language FlexibleInstances     #-}
{-# language FlexibleContexts      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language Rank2Types            #-}
{-# language PolyKinds             #-}
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
module Test.LessArbitrary(
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
import GHC.Generics as Generic
import GHC.TypeLits
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Arbitrary as QC
import Data.Hashable

<<costgen>>

-- Mark a costly constructor with this instead of `<$>`
(<$$$>) :: (a -> b) -> CostGen a -> CostGen b
costlyConstructor <$$$> arg = do
  spend 1
  costlyConstructor <$> arg

<<spend>>

<<budget>>


withCost :: Int -> CostGen a -> QC.Gen a
withCost cost gen = runCostGen gen
  `State.evalStateT` Cost cost

<<generic-instances>>

<<generic-less-arbitrary>>

<<less-arbitrary-class>>

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

<<arbitrary-implementation>>

flatLessArbitrary :: QC.Arbitrary a
              => CostGen a
flatLessArbitrary  = CostGen $ lift QC.arbitrary

instance LessArbitrary                a
      => LessArbitrary (Vector.Vector a) where
  lessArbitrary = Vector.fromList <$> lessArbitrary

<<lifting-arbitrary>>

```

# Appendix: lifting classic `Arbitrary` functions

```{.haskell #lifting-arbitrary file=lifting.hs}

instance QC.Testable          a
      => QC.Testable (CostGen a) where
  property = QC.property
           . sizedCost

forAll gen prop = do
  gen >>= prop

instance LessArbitrary  a
      => LessArbitrary [a] where
  lessArbitrary = pure [] $$$? do
    len  <- choose (1,100) -- FIXME: use sized
    spend $ Cost len
    replicateM   len lessArbitrary

oneof   :: [CostGen a] -> CostGen a
oneof [] = error "LessArbitrary.oneof used with empty list"
oneof gs = choose (0,length gs - 1) >>= (gs !!)

elements :: [a] -> CostGen a
elements gs = (gs!!) <$> choose (0,length gs - 1)

choose      :: Random  a
            =>        (a, a)
            -> CostGen a
choose (a,b) = CostGen $ lift $ QC.choose (a, b)

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
```

# Appendix: test suite {.unnumbered}


```{.haskell file=test/lib/Test/Arbitrary.hs}
{-# language DataKinds             #-}
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
module Test.Arbitrary where

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
import Test.QuickCheck.Classes

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
  Laws "arbitrary" [("does not shrink to itself", property (shrinkCheck :: ty -> Bool))]
```

# Appendix: test suite

```{.haskell file=test/less/LessArbitrary.hs}
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
{-# language DeriveGeneric         #-}
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
import Test.QuickCheck.Classes

import Test.Arbitrary

<<tree-type>>

main = lawsCheckMany [("Tree", [arbitraryLaws (Proxy :: Proxy (Tree Int))])]
```
