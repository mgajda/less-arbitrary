
#### Simple type constraints

1. Given a sample of values, we can have a reasonable approximation of expected values:
  - use `String` versus `Int` outright, instead of any JSON `Value`.
  - assuming that we have a set of parsers that are mutually exclusive, we can implement this for `String` values:

#### Constraints on string type

``` {.haskell #basic-constraints}
data StringConstraint =
    SCDate
  | SCEmail
  | SCEnum  (Set Text)
  | SCNever
  | SCAny
  deriving(Eq, Show,Generic)

instance StringConstraint `Types` Text where
  infer (isValidDate  -> True) = SCDate
  infer (isValidEmail -> True) = SCEmail
  infer  value                 = SCEnum $
                      Set.singleton value

  check  SCDate     s = isValidDate  s
  check  SCEmail    s = isValidEmail s
  check (SCEnum vs) s = s `Set.member` vs
  check  SCNever    _ = False
  check  SCAny      _ = True
```

Then whenever unifying the `String` constraint:
```{.haskell #basic-constraints}
instance Semigroup StringConstraint where
  SCNever    <>  a             = a
  a          <>  SCNever       = a
  SCAny      <>  _             = SCAny
  _          <>  SCAny         = SCAny
  SCDate     <>  SCDate        = SCDate
  SCEmail    <>  SCEmail       = SCEmail
  (SCEnum a) <> (SCEnum b)     |
          length (a <> b) < 10 = SCEnum (a <> b)
  _          <>  _             = SCAny

instance Monoid StringConstraint where
  mappend = (<>)
  mempty  = SCNever

instance Typelike StringConstraint where
  beyond  = (==SCAny)
```

#### Constraints on number type

Analogically we may infer for integer constraints^[Program makes it optional `--infer-int-ranges`.] as:
```{.haskell #basic-constraints}
data IntConstraint = IntRange Int Int
                   | IntNever
                   | IntAny
  deriving (Show, Eq, Generic)

instance Semigroup IntConstraint where
  IntAny       <> _            = IntAny
  _            <> IntAny       = IntAny
  IntNever     <> a            = a
  a            <> IntNever     = a
  IntRange          a         b  <>
    IntRange          c         d =
      IntRange (min a c) (max b d)

instance Typelike IntConstraint where
  beyond = (==IntAny)

instance Monoid IntConstraint where
  mempty = IntNever

instance IntConstraint `Types` Int where
  infer                i = IntRange i i
  check  IntNever      _ = False
  check  IntAny        _ = True
  check (IntRange a b) i = a <= i && i <= b
```

JavaScript has one number type that holds both `Float` and `Int`, so JSON inherits that:
```{.haskell #basic-constraints}
data NumberConstraint =
    NCInt
  | NCNever
  | NCFloat
  deriving(Eq,Show,Generic)

instance Semigroup NumberConstraint where
  <<standard-rules-number-constraint>>
  NCInt <> NCInt = NCInt

instance Typelike NumberConstraint where
  beyond = (==NCFloat)

instance NumberConstraint `Types` Scientific where
  infer sci
    | base10Exponent sci >= 0 = NCInt
  infer sci                   = NCFloat
  check NCFloat sci = True
  check NCInt   sci = base10Exponent sci >= 0
  check NCNever sci = False

<<standard-instances-number-constraint>>
```

```{.haskell #standard-rules-number-constraint}
NCFloat <> _       = NCFloat
_       <> NCFloat = NCFloat
NCNever <> a       = a
a       <> NCNever = a
```

```{.haskell .hidden #standard-instances-number-constraint}
instance Monoid NumberConstraint where
  mempty = NCNever
```

## Free union type

Now for a term with constructors we can infer "free" type for every term:
For any `T` value type Set T` satisfies our notion of _free type_.
``` { .haskell #freetype }
data FreeType a = FreeType { captured :: Set a }
                | Full
  deriving (Eq, Ord, Show, Generic)

instance (Ord a, Eq a) => Semigroup (FreeType a) where
  Full <> _    = Full
  _    <> Full = Full
  a    <> b    = FreeType
               $ (Set.union `on` captured) a b
instance (Ord a, Eq a) => Monoid (FreeType a) where
  mempty  = FreeType Set.empty

instance (Ord a, Eq a, Show a)
      => Typelike (FreeType a) where
  beyond    = (==Full)

instance (Ord      a
         ,Eq       a
         ,Show     a)
      =>  FreeType a `Types` a where
  infer                    = FreeType . Set.singleton
  check Full         _term = True
  check (FreeType s)  term = term `Set.member` s
```


This definition is sound, and for a finite realm of values, may make a sense.
For a set of inputs:
`["yes", "no", "error"]`, we might reasonably say that type is indeed
a good approximation of C-style enumeration, or Haskell-style ADT without constructor arguments.

What is wrong with this notion of _free type_?
It does not generalize at all to infinite and recursive domains! It only allows the objects from the sample, and nothing more.

## Presence and absence constraint

We call this useful case a _presence or absence constraint_:
```{.haskell #presence-absence-constraint}
data PresenceConstraint a =
    Present
  | Absent
  deriving (Eq, Show)

instance Typelike (PresenceConstraint a) where
  mempty = Absent
  beyond    = Present

instance Monoid (PresenceConstraint a) where
  Absent  <> a       = a
  a       <> Absent  = a
  Present <> Present = Present

instance PresenceConstraint a `Types` a where
  infer _         = Present
  check Present _ = True
  check Absent  _ = False
```

While it does not look immediately useful
in context where we always have at least one value
on input, it is important where we can receive empty
array (and thus no values for the element type.)

### Which of these types we may infer?

#### Presence and absence constraints

After seeing `true` value we also expect
`false`, so we can say that the basic constraint
for a boolean value is its presence or absence.

``` {.haskell #presence-absence-constraints}
type BoolConstraint = PresenceConstraint Bool

type role PresenceConstraint nominal

data PresenceConstraint a =
    Present -- ^ some values seen
  | Absent  -- ^ no information
  deriving (Eq,Show,Generic)

instance Semigroup (PresenceConstraint a) where
  Present <> _       = Present
  _       <> Present = Present
  Absent  <> Absent  = Absent

instance Monoid (PresenceConstraint a) where
  mempty = Absent

instance Typelike (PresenceConstraint a) where
  beyond = (==Present)

instance PresenceConstraint a `Types` a where
  infer _ = Present
  check Absent  _ = False
  check Present _ = True
```
Here we have basic presence constraint for any non-Void type:
(...repetition...)

Note that booleans and `null` values are
both denoted by this trivial `PresenceConstraint`
constraint.

In other words, we only
care about presence or absence of their observation.
That means that constraint for boolean
is the simplest possible.

The same for `null`, since there is only one
`null` value.

``` {.haskell #presence-absence-constraints}
type NullConstraint = PresenceConstraint ()
```

### Selecting basic priors

Now that we defined the type system engineering
as prior selection, let's state some obvious rules
for the typing:

We generalize basic datatypes.

Note that we treat `null` as separate basic types,
that can form union with any other.
Thus `mempty` indicates _no type and no value_.
The `beyond` of our semilattice is the type of any `Value` term.

#### Variants

Variant fields for union types are also simple, we implement them with a cousin of `Either` type
that assumes these types are exclusive:
```haskell
data a :|: b = AltLeft  a
             | AltRight b
  deriving (Show, Eq, Generic)

instance (FromJSON  a
         ,FromJSON        b)
      =>  FromJSON (a :|: b) where
  parseJSON a =  AltLeft  <$> decodeEither
             <|> AltRight <$> decodeEither
```
In other words for `Int :|: String` type we first check if the value is `String`, and if it fails try to parse it as `String`.

Variant records are a bit more complicated, since it is unclear which typing is better:

```{.json .javascript file=test/example_variant1.json}
{"message": "Where can I submit my proposal?",
    "uid" : 1014}
{"error"  : "Authorization failed",
   "code" : 401}
```

```{.haskell file=test/example_variant1.result}
data OurRecord =
  OurRecord { message :: Maybe String
            , error   :: Maybe String
            , code    :: Maybe Int
            , uid     :: Maybe Int }
```

Or maybe:
```{.haskell file=test/example_variant2.result}
data OurRecord2 = Message { message :: String
                          , uid     :: Int }
                | Error   { error   :: String
                          , code    :: Int }
```

The best attempt here, is to rely on our examples being reasonable exhaustive.
That is, we can count how many examples we have for each, and how many out of them
are matching. And then compare it to type complexity (with optionalities being more complex than lack of them.)
In this case, latter definition has only one choice (optionality), but we only have two samples to begin with.

With more samples, the pattern emerges:
```{.json file=test/example_variant2.json}
{"error"  : "Authorization failed",
    "code":  401}
{"message": "Where can I submit my proposal?",
    "uid" : 1014}
{"message": "Sent it to HotCRP",
    "uid" :   93}
{"message": "Thanks!",
    "uid" : 1014}
{"error"  : "Missing user",
    "code":  404}
```

### Object constraint

In order to avoid information loss,
constraint for JSON object type should
**simultaneously gather information** about
either representing it as a `Map`, or
a record:
```{.haskell #object-constraint}

data MappingConstraint =
  MappingConstraint {
      keyConstraint   :: StringConstraint
    , valueConstraint :: UnionType
    } deriving (Eq, Show, Generic)

instance Monoid MappingConstraint where
  mempty = MappingConstraint {
      keyConstraint   = mempty
    , valueConstraint = mempty
    }

instance Typelike MappingConstraint where
  beyond MappingConstraint {..} =
       beyond keyConstraint
    && beyond valueConstraint

instance Semigroup   MappingConstraint where
  a <> b = MappingConstraint {
      keyConstraint   =
        ((<>) `on` keyConstraint  ) a b
    , valueConstraint =
        ((<>) `on` valueConstraint) a b
    }

instance MappingConstraint `Types`
         Object where
  infer obj =
    MappingConstraint
      (foldMap infer $ Map.keys obj)
      (foldMap infer            obj)
  check (MappingConstraint {..}) obj =
       all (check keyConstraint)
           (Map.keys obj)
    && all (check valueConstraint)
           (Foldable.toList obj)
```

Separately we gather information about
the possible typing of the JSON object
as a record of values:
```{.haskell #object-constraint}
data RecordConstraint =
    RCTop
  | RCBottom
  | RecordConstraint {
        fields :: HashMap Text UnionType
      } deriving (Show,Eq,Generic)

instance Typelike RecordConstraint where
  beyond = (==RCTop)

instance Semigroup   RecordConstraint where
  RCBottom <> a        = a
  a        <> RCBottom = a
  RCTop    <> _        = RCTop
  _        <> RCTop    = RCTop
  a        <> b        = RecordConstraint $
    Map.unionWith (<>) (fields a)
                       (fields b)

instance Monoid      RecordConstraint where
  mempty = RCBottom

instance RecordConstraint `Types` Object
  where
    infer =  RecordConstraint
          .  Map.fromList
          .  fmap (second infer)
          .  Map.toList
    check RCTop    _ = True
    check RCBottom _ = False
    -- FIXME: treat extra keys!!!
    check rc obj
            | Map.keys (fields rc)
           == Map.keys        obj  =
      and $ Map.elems $
        Map.intersectionWith  check
                             (fields rc)
                              obj
    check _  _ = False
```

Seeing that the two abstract domains above
are independent, we store information
about both option separately in a record
^[Choice of representation will be explained later.
Here we only consider gathering of information
about the possible values.]

```{.haskell #object-constraint}
data ObjectConstraint = ObjectConstraint {
    mappingCase :: MappingConstraint
  , recordCase  :: RecordConstraint
  } deriving (Eq,Show,Generic)

instance Semigroup ObjectConstraint where
  a <> b =
    ObjectConstraint {
      mappingCase =
        ((<>) `on` mappingCase) a b
    , recordCase =
        ((<>) `on` recordCase ) a b
    }

instance Monoid ObjectConstraint where
  mempty = ObjectConstraint {
             mappingCase = mempty
           , recordCase  = mempty
           }

instance Typelike ObjectConstraint where
  beyond ObjectConstraint {..} =
       beyond mappingCase
    && beyond recordCase

instance ObjectConstraint `Types` Object where
  infer v = ObjectConstraint (infer v)
                             (infer v)
  check ObjectConstraint {..} v =
       check mappingCase v
    && check recordCase  v
```

Note that this representation is similar
in sense to _intersection type_:
any value that satisfies `ObjectConstraint`
must satisfy by contained `mappingCase`,
and `recordCase`.

Note that this _intersection approach_
to conflicting union type constraints enjoys
_principal type property_ -- principal
type simply gathers information from different
angles and handles it separately.

### Array constraint

Similarly to the object,
`ArrayConstraint` should simultaneously gather
information about all possible representations
of the array:

* an array of the same elements,
* row with the type depending on the column.

We need to gather information for both
alternatives separately, and then
measure relatively likelihood
of either case just before mapping
the union type to Haskell declaration.

Again, we put the record of
two different possible representations:
```{.haskell #array-constraint}

data ArrayConstraint  = ArrayConstraint {
    arrayCase :: UnionType
  , rowCase   :: RowConstraint
  }
  deriving (Show, Eq, Generic)

instance Monoid ArrayConstraint where
  mempty = ArrayConstraint {
             arrayCase = mempty
           , rowCase   = mempty
           }

instance Typelike ArrayConstraint where
  beyond ArrayConstraint {..} =
       beyond arrayCase
    && beyond rowCase

instance Semigroup ArrayConstraint where
  a1 <> a2 =
    ArrayConstraint {
      arrayCase = ((<>) `on` arrayCase) a1 a2
    , rowCase   = ((<>) `on` rowCase  ) a1 a2
    }

<<row-constraint>>

instance ArrayConstraint `Types` Array
  where
    infer vs =
      ArrayConstraint
        (mconcat (infer <$>
               Foldable.toList vs))
        (infer              vs)
    check ArrayConstraint {..} vs =
         and (check arrayCase <$>
                Foldable.toList vs)
      && check rowCase   vs

```

### Row constraint

Row constraint is valid only if
there is a fixed number of entries in each
row, which we represent by escaping to the `beyond`
whenever there is uneven number of columns.

``` {.haskell #row-constraint}
data RowConstraint =
     RowTop
   | RowBottom
   | Row       [UnionType]
   deriving (Eq,Show,Generic)

instance Typelike RowConstraint where
  beyond = (==RowTop)

instance Monoid RowConstraint where
  mempty = RowBottom

instance RowConstraint `Types` Array where
  infer = Row
        . Foldable.toList
        . fmap infer
  check RowTop    _ = True
  check RowBottom _ = False
  check (Row rs) vs
    | length rs == length vs =
      and $
        zipWith check                 rs
                     (Foldable.toList vs)
  check  _        _ = False

instance Semigroup RowConstraint where
  RowTop    <> _             = RowTop
  _         <> RowTop        = RowTop
  RowBottom <> a             = a
  a         <> RowBottom     = a
  Row bs    <> Row cs
    | length bs /= length cs = RowTop
  Row bs    <> Row cs        =
    Row $ zipWith (<>) bs cs
```

In other words, `RowConstraint` is a _levitated
semilattice_[@levitated-lattice]
with neutral element over content
type `[UnionType]`.

```{.haskell #row-constraint-standard-rules .hidden}
  RowBottom <> r         = r
  r         <> RowBottom = r
  RowTop    <> _         = RowTop
  _         <> RowTop    = RowTop
```

### Putting it together into a union

Since we have the union of just the few possible constraints,
we make it a record for easier processing:

Note that given constraints for the different type
constructors, the union type can be though of
as mostly a generic monoid[@generic-monoid]:

```{.haskell #type}
data UnionType =
  UnionType {
    unionNull :: NullConstraint
  , unionBool :: BoolConstraint
  , unionNum  :: NumberConstraint
  , unionStr  :: StringConstraint
  --, unionArr  :: ArrayConstraint
  , unionArr  :: PresenceConstraint Array --ArrayConstraint
  , unionObj  :: PresenceConstraint Object --ObjectConstraint
  --, unionObj  :: ObjectConstraint
  }
  deriving (Eq,Show,Generic)

instance Semigroup UnionType where
  u1 <> u2 =
    UnionType {
      unionNull = ((<>) `on` unionNull) u1 u2
    , unionBool = ((<>) `on` unionBool) u1 u2
    , unionNum  = ((<>) `on` unionNum ) u1 u2
    , unionStr  = ((<>) `on` unionStr ) u1 u2
    , unionObj  = ((<>) `on` unionObj ) u1 u2
    , unionArr  = ((<>) `on` unionArr ) u1 u2
    }
```
Generic structure of union type can be explained
by the fact that informations contained in different
record fields are _independent from each other_.
That means that we compute the meet over different dimensions.

```{.haskell #type}
instance Monoid UnionType where
  mempty = UnionType {
      unionNull = mempty
    , unionBool = mempty
    , unionNum  = mempty
    , unionStr  = mempty
    , unionObj  = mempty
    , unionArr  = mempty
    }
```

Since we described `beyond` set that is
either **accepting any value**,
and **accepting no more information**,
its definition should be no surprise:

```{.haskell #type}
instance Typelike UnionType where
  beyond UnionType {..} =
      beyond unionNull
   && beyond unionBool
   && beyond unionNum
   && beyond unionStr
   && beyond unionObj
   && beyond unionArr
```

Inference breaks disjoint alternatives
to different record fields,
depending on the constructor of a given value.

This allows as for clear and efficient
treatment of values distinguished by different
dynamic type into different partitions
of the union^[Impatient reader could ask: what is the _union type_ without _set union_? When the sets are disjoint, we just put the values in different bins for easier handling.]

``` {.haskell #union-type-instance}
instance UnionType `Types` Value where
  infer (Bool   b) = mempty { unionBool = infer b  }
  infer  Null      = mempty { unionNull = infer () }
  infer (Number n) = mempty { unionNum  = infer n  }
  infer (String s) = mempty { unionStr  = infer s  }
  infer (Object o) = mempty { unionObj  = infer o  }
  infer (Array  a) = mempty { unionArr  = infer a  }
  check UnionType { unionBool } (Bool   b) =
              check unionBool          b
  check UnionType { unionNull }  Null      =
              check unionNull         ()
  check UnionType { unionNum  } (Number n) =
              check unionNum            n
  check UnionType { unionStr  } (String s) =
              check unionStr            s
  check UnionType { unionObj  } (Object o) =
              check unionObj            o
  check UnionType { unionArr  } (Array  a) =
              check unionArr            a
```

### Overlapping alternatives

Crux of union type systems have been long
dealing with conflicting types on the input.

Motivated by examples above, we want to also deal
with conflicting alternative assignments.

It is apparent that examples 4. to 6.
hint at more than one assignment:

5. Either a list of lists of values that are one of `Int`, `String`, or `null`, or a table that has the same (and predefined) type
for each

6. Either a record of fixed names,
or the mapping from hash to a single object type.

### Counting observations

How can we make sure that we have a right number of samples?
This is another example:
```json
{"samples":
[{"error"  : "Authorization failed",
  "code"   :  401}
,{"message": "Where can I submit my proposal?",
     "uid" : 1014}
,{"message": "Sent it to HotCRP",
     "uid" :   93}
,{"message": "Thanks!",
     "uid" : 1014}
,{"error"  : "Authorization failed",
     "code":  401}
]}
```
First we need to identify it as a list of same elements,
and then to notice, that there are multiple instances of each record example.
That suggests that the best would be to use not sets, but multisets
of inferred records, and attempt to minimize the term.

Next is detection of similarities between type descriptions developed
for different parts of the term:
```json
{"samples"      :  [...],
 "last_message" : {"message": "Thanks!",
                      "uid" : 1014}
}
```
We can add auxiliary information about number of samples seen
and the constraint will stay `Typelike`:

```{.haskell #counted }
data Counted a =
  Counted { count      :: Int
          , constraint :: a
          } deriving (Eq, Show)

instance Semigroup          a
      => Semigroup (Counted a) where
  a <> b = Counted (count      a +  count      b)
                   (constraint a <> constraint b)

instance Monoid  a
      => Monoid (Counted a) where
  mempty = Counted 0 mempty

instance Typelike          a
      => Typelike (Counted a) where
  beyond Counted {..} = beyond constraint
```

We can connect `Counted` as parametric functor
to our types in order to track auxiliary
information.

Note that the `Counted` constraint is the
first example of constraint that is not
a semilattice, that is `a<>a/=a`.

This is because it is `Typelike`, but
it is not a type constraint in a traditional sense,
instead it counts the samples observed for the
constraint inside, so we can decide
which alternative representation is best supported
by evidence.

Thus at each step we might want to keep a **cardinality** of each possible value,
and given enough samples, attempt to detect patterns ^[If we detect pattern to early, we risk make our types to narrow to work with actual API answers.].

In order to preserve efficiency, we might want to merge whenever, number of alternatives in the multiset crosses the threshold.
^[Option `--max-alternative-constructors=N`]
And only attempt to narrow strings when cardinality crosses the threshold ^[Option `--min-enumeration-cardinality`.]
