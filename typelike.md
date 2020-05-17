
## Defining type inference

If inference fails, we can always correct it by adding additional example.

Minimal definition of the typing inference and checking relations

```{.haskell file=refs/Data/Monoid.hs}
class Semigroup ty where
  (<>) :: ty -> ty -> ty

class Semigroup ty
   => Monoid    ty
  where
    mempty :: ty
```

We describe laws as QuickCheck[@quickcheck]
properties so that unit testing can detect
obvious violations.

We use `validity-properties` package[@validity]
for common properties:
```{.haskell #typelike-spec}
commutativeSemigroupSpec :: forall       ty.
                           (Semigroup    ty
                           ,Show         ty
                           ,Eq           ty
                           ,Arbitrary    ty
                           )
                         => Spec
commutativeSemigroupSpec = do
  prop "commutative" $
        commutativeOnArbitrary @ty (<>)
  prop "associative" $
        associativeOnArbitrary @ty (<>)
```

Neutral element of the `Typelike` monoid,
`mempty` stands for **no information accepted** about possible value
 (no term seen,
  not even a `null`).
  For example an empty array `[]` could be typed
  as a array type with `mempty` as element type.

In the domain of permissive union types,
`beyond` represents **everything permitted**
or fully dynamic value, when we gathered
information that places any possible value inside the type.
At first reading one may be tempted to claim
that `beyond` set should consists of only a single element -- the `top`.

However, since we defined **unification**
operator `<>`, as **information fusion**,
we might have difficulty in assuring
that no information is lost during our unification.

Also strict type systems tend to have more than
one error value -- since it has to keep the
error message, and track of where the error
originated^[In this case: `beyond (Error _) = True | otherwise = False`.].

This firmly places type inference as a **learning problem**,
and allows us to find the common ground
between dynamic and static typing disciplines.

Languages with static type discipline usually
treat `beyond` as a set of error messages,
since value should have
a statically assigned and **narrow** type,
and `mempty` as a fully polymorphic
type `forall a. a`.

Languages with dynamic type discipline will
treat `beyond` as untyped, dynamic value,
and `mempty` again as an unknown polymorphic
value^[May sound similar until we consider adding
more information to the type.].

Note that `Monoid` operation is a union type unification.

Beside standard laws for monoid
with `beyond` set closed to information addition
by `(<>a)` or `(a<>)` for any value of `a`.

Since we state it as **information acquisition**,
we will use a better interface:
```{.haskell #typelike}
class (Monoid   t
      ,Eq       t
      ,Show     t)
   =>  Typelike t where
   beyond :: t -> Bool
```

That is because we want more than a single `beyond`
element. When typing Haskell, we would
like `beyond` to contain error message,
which makes many.^[Note that many, but not all
type constraints will be semilattice. See counting example below.]

Still we find it useful to state which `Typelike`s
instances are semilattices:
```
idempotentSemigroupSpec :: forall    ty.
                           Semigroup ty
                        => Spec
idempotentSemigroupSpec  = do
  prop (nameOf @ty <> "is idempotent semigroup") $
    idempotentSemigroup @ty

idempotentOperation :: forall ty.
                       Semigroup ty
                    => ty -> Bool
idempotentOperation op a = (a `op` a) `shouldBe` a
```
In case of union types, we accept that there
are many elements in the `beyond` set.
Key property of the `beyond` set, is that it
is closed to information acquisition:
```{.haskell #typelike-spec}

beyond_is_closed :: forall   ty.
                    Typelike ty
                 => ty -> ty -> Property
beyond_is_closed ty1 ty2 = do
  beyond (ty1 :: ty) ==> beyond (ty1 <> ty2)

typelikeLaws (Proxy :: Proxy a) =
  Laws "Typelike"
    [("beyond is closed",
      property $ beyond_is_closed @a)]
```

It is convenient validation when testing a recursive structure of the type.

Note that we abolish semilattice requirement
that was traditionally assumed for
type constraints here[@semilattice].

That is because this requirement is valid
only for strict type constraint inference,
not for a more general type inference as a learning problem.
As we saw in the example @example:row-constraint,
we need non-monotonic inference when dealing
with alternative representations.

### Typing relation

```{.haskell #typelike}
class Typelike ty
   => ty `Types` val where
   infer ::       val -> ty
   check :: ty -> val -> Bool
```

Here:

* `<>` is unification: associative, commutative operation
* `mempty` is neutral element of unification
* `beyond` set is an attractor of `<>` on both sides
^[So both `forall a. (<> a)` and ∀`a.(a<>)`
  are keep result in the `beyond` set.]

In case of union types, where we learn shape of the type
from examples, we can say that:

* `mempty` represents _no information received_
* `beyond` represents sets of typelikes where
   _no more information will change anything_
or _no more information is accepted_
(for the purpose of typing; the extra
 information may be still useful in error messages)
* `<>` represents fusing information
  on the same entity
  from different observations

We call neutral element to be _no information given_
or _no observation_, and maximum to be
_no more information accepted_.

#### Laws of typing

This is important, since we may
want to separate concerns about
valid domain of types/type constraints
versus concerns of the typing of the terms
by these valid types.

Note that this approach is a bit more relaxed than
full lattice subtyping[@subtype-inequalities][@subtyping-lattice],
since it only considers semilattice with unification operation.



## Laws of typing

```{.haskell #types-spec .hidden}

typesSpec :: forall ty         v.
                   (ty `Types` v
                   ,Typelike ty
                   ,Arbitrary  v
                   ,Show       v
                   ,Arbitrary  ty
                   ,Show       ty
                   ,Typeable   ty)
          => Spec
typesSpec = do
  describe ("Types " <> nameOf @ty) $ do
    prop "mempty contains no terms" $
      mempty_contains_no_terms        @ty @v
    prop "beyond contains all terms" $
      beyond_contains_all_terms       @ty @v
    prop "inferred type always contains its term" $ do
      inferred_type_contains_its_term @ty @v
    prop (nameOf  @ty <> "fusion keeps terms") $
      fusion_keeps_terms @ty @v

```
First we note that to describe _no information_,
`mempty` cannot correctly type any term:

```{.haskell #types-spec}
mempty_contains_no_terms
  :: forall    ty term.
     (Typelike ty
     ,Types    ty term)
  =>              term
  -> Expectation
mempty_contains_no_terms term =
      check (mempty :: ty) term
        `shouldBe` False
```

It is also important for typing:
all terms are typed successfully by any value `beyond`.
```{.haskell #types-spec}
beyond_contains_all_terms :: forall ty    term.
                            (Types  ty    term
                            ,Show         term)
                          =>        ty -> term
                          -> Property
beyond_contains_all_terms ty term = do
  beyond ty ==>
    term `shouldSatisfy` check ty
```

However getting types `beyond` may be non-obvious for some instances.
In this case we would use special generator `arbitraryBeyond`:
```{.haskell #types-spec}

{-
beyond_contains_all_terms2 :: forall   ty term.
                             (Typelike ty
                             ,Types    ty term)
                           => term -> _
beyond_contains_all_terms2 term =
  forAll arbitraryBeyond $ (`check` term)
  -}
```

For typing we have additional rule:
type inferred from a term, must always be valid
for the very same term.
``` {.haskell #types-spec}
inferred_type_contains_its_term ::
     forall ty         term.
            ty `Types` term
  =>                   term
  -> Bool
inferred_type_contains_its_term term =
  check ((infer:: term -> ty) term) (term :: term)
```

Law asserts that the following diagram commutes:

```{ .dot width=45% height=20% #fig:type-commutes }
digraph type {
  node [shape=box,color=white];
  subgraph g {
    Bool; Type;
    rank=same;
  }
  Value -> Type [xlabel="infer"];
  Type  -> Bool [label="check with value"];
  Value -> Bool [label="const True"];
}
```

The last law states that the terms are
still correctly type checked after fusing
more information into the type:

```{.haskell #types-spec }
fusion_keeps_terms :: forall   ty v.
                     (Typelike ty
                     ,ty `Types` v)
                   => v -> ty -> ty -> Property
fusion_keeps_terms v ty1 ty2 = do
  check ty1 v || check ty2 v ==>
    check (ty1 <> ty2) v

```

Minimal `Typelike` instance would be one
that contains only `mempty` for _no sample data received_,
and `beyond` for _all values permitted_.

Note that these laws are still compatible
with strict, static type discipline:
`beyond` set is a set of type errors then,
and it is a task of a compiler to disallow
any program with terms that type only to `beyond`
as a least upper bound.

## Type engineering principles

Given that we want to infer the type from finite number of samples
we are presented with _learning problem_,
so we need to use _prior_ knowledge of the domain
to generalize when inferring types.

Clearly after seeing `a: false` we can expect that it is sometimes `a: true`.
After seeing `b: 123` we expect that `b: 100` would also be acceptable.
That means that we need our typing to _learn a reasonable general class from few instances._
That defines making a practical type system as inference problem.

Since our goal is to deliver most descriptive^[Shortest, by information complexity principle.]
types, we will assume that we need to abstract a bit from the _free type_ and take on larger
sets whenever it seems justified.

Another principle is that of **correct operation**,
where given operations on types, we try to find a minimal types
that assure correct operation unexpected errors.

Indeed we want to use this theory to infer a type definition from a finite set of examples,
but we also want it to generalize to infinite types.

Aiming for this, we set the rules of type design:

* type should have a finite description
* inference must be contravariant functor with regards to constructors, for `{"a": X, "b": Y}` is types
  by `T x y`,
  then `X :: x` and `Y :: y` must be also a valid typing.
