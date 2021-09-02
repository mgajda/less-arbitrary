---
title:  "Less arbitrary waiting time"
author:
  - Michał J. Gajda
  - "https://www.migamake.com"
author-meta: "Migamake Pte Ltd"
tags:
  - Haskell
  - generics
  - property testing
  - QuickCheck
abstract: |
  Property testing is the cheapest and most precise way of building up a test suite for your program.
  Especially if the datatypes enjoy nice mathematical laws.
  But it is also the easiest way to make it run for an unreasonably long time.
  We show a simple way to add state to generators,
  and simultaneously make them run in an expected linear time.

  The method has a bonus of detecting non-terminating example generation loops,
  and reporting them.
date:   2021-09-03
link: "https://gitlab.com/migamake/philosophy-articles/towards-better-union"
bibliography:
  - multicloud-binding.bib
tables: true
listings: true
acks: |

  Migamake Pte Ltd sponsored the effort.

---

# Plan

* Property testing
* Agile
* Problems with generators
* Generic solution

# Property testing

::: {.incremental}

* Tests on sets not values
* Less work to make exhaustive tests
* Problem with recursive data structures

```{.haskell}
prop_showRead :: MyType -> Bool
prop_showRead x = read (show x) == x

main = quickCheck prop_showRead
```
```
> +++ OK, passed 100 tests.
```

:::

# Problem with generators

```{.haskell}
data MyType =
    Add   MyType MyType
  | Mul   MyType MyType
  | Const Int
  deriving (Eq, Ord, Show,Read,Generic)

instance Arbitrary Expr where
  -- default method
```

. . .

```
<<loop>>
```

. . .

_Branching factor 2x for 2 of 3 constructors_

`HSExpr` has 30 constructors and crazy branching factor..

# Automatic generator

```{.haskell}
instance Arbitrary Expr where
  arbitrary =
    oneOf [
           Add   <$> arbitrary <*> arbitrar

          ,Mul   <$> arbitrary <*> arbitrary 
           
          ,Const <$> arbitrary]
```

# Automatic generator -- analysis

```{.haskell}
instance Arbitrary Expr where
  arbitrary =
    oneOf [-- Doubles:
           Add   <$> arbitrary <*> arbitrary
           -- Doubles:
          ,Mul   <$> arbitrary <*> arbitrary 
           -- Terminates
          ,Const <$> arbitrary]
```

# Manual generator 1

```{.haskell}
instance Arbitrary Expr where
  arbitrary =
    frequency [(1, Add   <$> arbitrary <*> arbitrary)
              ,(1, Mul   <$> arbitrary <*> arbitrary)
              ,(3, Const <$> arbitrary              )]
```

. . .

_Termination probability is greater than branching factor._

# Manual generator 2

```{.haskell}
instance Arbitrary Expr where
  arbitrary = sized $ \n ->
    if n <= 1
      then Const <$> arbitrary
      else resize (n/2) $ do
             oneOf [Add   <$> arbitrary <*> arbitrary
                   ,Mul   <$> arbitrary <*> arbitrary
                   ,Const <$> arbitrary]
```

_Explicit termination count._

# Agile software development

::: {.incremental}

* Maximizing outcome
* Minimizing effort
* Choosing outcome
* ... finding manager who knows it

:::

# Problems with property testing

Easier to test on sets, but...

::: {.incremental}

* More time spent
* Effort in manual generators
* Looping forever is bad practice
* _Async-based test runner will not even give error message_

:::

# Goal

::: {.incremental}

* Maximize test coverage with property testing
* Minimum effort to write generators
* Always terminate
* Work for mutually recursive data structures

:::

# Solution

```{.haskell}
instance LessArbitrary MyType where

instance _ => Arbitrary MyType where
  arbitrary = fasterArbitrary
```

# How we solve it?

::: {.incremental}

* State monad tracking cost of generated structure
* Generic detects terminating constructors
* _Bonus:_
  - expected size of structure
  - ignore branching factor

:::

# Solution: monad

```{.haskell #costgen}
newtype Cost = Cost Int 
  deriving (Eq,Ord,Enum,Bounded,Num)

newtype CostGen                             s         a =
        CostGen {
          runCostGen :: State.StateT (Cost, s) QC.Gen a }
  deriving (Functor, Applicative, Monad, State.MonadFix)

spend :: Cost -> CostGen ()
spend c = CostGen $ State.modify (\(b, s) -> (b-c, s))
```

# Solution: budget check operator

To make generation easier, we introduce `budget check` operator:
```{.haskell #budget}
($$$?) :: CostGen a
       -> CostGen a
       -> CostGen a
cheapVariants $$$? costlyVariants = do
  budget <- CostGen State.get
  if | budget > (0 :: Cost) -> costlyVariants
     | budget > -10000      -> cheapVariants
     | otherwise            -> error $
       "Recursive structure with no loop breaker."
```

The operator also reports non-terminating example generation.

# Solution with class

```{.haskell}
class LessArbitrary                        s      a where
  lessArbitrary         :: CostGen         s      a
  default lessArbitrary :: (Generic               a
                           ,GLessArbitrary s (Rep a))
                        =>  CostGen        s      a
  lessArbitrary = genericLessArbitrary
```

# Generic implementation


```{.haskell #generic-less-arbitrary}
class GLessArbitrary        s  datatype where
  gLessArbitrary :: CostGen s (datatype p)
  cheapest       :: CostGen s (datatype p)
```

# Summary

::: incremental

* Fixes Arbitrary to make it predictable
* Generics make it agile
* State argument for extra data in generator
* Error message in case of loop
* Simplicity can be copied to other languages

:::