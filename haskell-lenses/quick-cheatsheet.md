## Basic Viewing & Setting

```haskell
-- View a field
record ^. fieldLens
view fieldLens record

-- Set a field
record & fieldLens .~ newValue
set fieldLens newValue record

-- Modify a field
record & fieldLens %~ (+1)
over fieldLens (+1) record

-- Modify with access to old value
record & fieldLens %%~ (\old -> (old, old + 1))  -- stateful
```

## Maybe Handling

```haskell
-- View Maybe with default
someMaybe ^. non defaultVal

-- View nested Maybe (returns Nothing if any level is Nothing)
record ^? field1 . field2 . field3

-- Set inside Maybe (no-op if Nothing)
someMaybe & _Just .~ newValue

-- Modify inside Maybe
someMaybe & _Just %~ (+1)

-- Convert Nothing to Just
Nothing & non defaultVal .~ newValue  -- Just newValue

-- Preview (safe head for lists, safe unwrap for Maybe)
someMaybe ^? _Just
someList ^? _head

-- Unsafe preview (throws error if nothing)
someMaybe ^?! _Just
```

## List Operations

```haskell
-- Get all elements
list ^.. traverse
toListOf traverse list

-- Get first element with default
list ^?! ix 0  -- unsafe
list ^? ix 0   -- safe, returns Maybe

-- Get nth element
list ^? ix 3

-- Set nth element
list & ix 3 .~ newValue

-- Modify nth element
list & ix 3 %~ (+1)

-- Filter and view
list ^.. folded . filtered (> 5)

-- Map over list
list & traverse %~ (+1)
list & each %~ (+1)  -- same for lists

-- Get first element
list ^? _head

-- Get last element
list ^? _last

-- Get tail
list ^? _tail

-- Set first element
list & _head .~ newValue

-- Modify all elements matching predicate
list & traverse . filtered (> 5) %~ (*2)

-- Take first n elements
list ^.. taking 3 traverse

-- Drop first n elements
list ^.. dropping 2 traverse

-- Reverse view
list ^.. backwards traverse
```

## Tuple Operations

```haskell
-- Access tuple fields
tuple ^. _1
tuple ^. _2
tuple ^. _3  -- up to _9

-- Set tuple fields
tuple & _1 .~ newValue

-- Swap tuple
tuple & swapped  -- (b, a)

-- Nested tuples
((1, 2), 3) ^. _1 . _2  -- 2
```

## Either Handling

```haskell
-- View Left
someEither ^? _Left

-- View Right
someEither ^? _Right

-- Set Right (no-op if Left)
someEither & _Right .~ newValue

-- Modify Right
someEither & _Right %~ (+1)

-- Check if Left/Right
has _Left someEither   -- Bool
has _Right someEither  -- Bool

-- Choose between Left and Right
someEither ^. choosing _Left _Right
```

## Map/Dictionary Operations

```haskell
-- Lookup key
map ^? ix "key"
map ^. at "key"  -- returns Maybe

-- Set key
map & at "key" ?~ newValue  -- Just newValue
map & at "key" .~ Just newValue

-- Delete key
map & at "key" .~ Nothing

-- Modify value at key
map & ix "key" %~ (+1)
map & at "key" . _Just %~ (+1)

-- Get all keys
map ^.. itraversed . asIndex

-- Get all values
map ^.. traverse
map ^.. folded

-- Get all key-value pairs
map ^@.. itraversed  -- [(key, value)]

-- Filter by key
map ^.. itraversed . indices (> "m")

-- Filter by value
map ^.. traverse . filtered (> 5)
```

## Nested Record Access

```haskell
-- Deep access
record ^. field1 . field2 . field3

-- Deep modification
record & field1 . field2 . field3 .~ newValue

-- Modify with context
record & field1 . field2 %~ (\x -> x + (record ^. field3))
```

## Conditional Operations

```haskell
-- Apply lens only if condition
record & field1 . filtered (> 5) .~ 10

-- Apply different lenses based on condition
record & choosing _Left _Right .~ value

-- Conditional modification
list & traverse . filtered even %~ (*2)

-- Only set if Nothing
maybeThing & non defaultVal .~ newValue
```

## Composition Patterns

```haskell
-- Compose lenses
lens1 . lens2 . lens3

-- Parallel composition (apply to both)
record & alongside field1 field2 %~ (\(a, b) -> (a+1, b+1))

-- Choice composition
beside _1 _2  -- for nested structures
```

## Advanced Getters

```haskell
-- To (custom getter)
record ^. to (\r -> r.field1 + r.field2)

-- Combining getters
record ^. to field1 <> to field2  -- requires Monoid

-- Index-aware traversal
list ^@.. itraversed  -- [(index, value)]

-- Get indices only
list ^.. itraversed . asIndex
```

## Folds and Multiple Values

```haskell
-- Fold all values with operation
sumOf (traverse . field) records
productOf (traverse . field) records

-- Any/All predicates
anyOf (traverse . field) (> 5) records
allOf (traverse . field) (> 5) records

-- Find first matching
records ^? traverse . filtered (\r -> r ^. field > 5)

-- Count matches
lengthOf (traverse . filtered condition) records

-- Maximum/Minimum
maximumOf (traverse . field) records
minimumOf (traverse . field) records

-- First/Last
firstOf traverse list
lastOf traverse list
```

## Indexed Operations

```haskell
-- Indexed traversal
list & itraverse %@~ (\i x -> show i ++ ": " ++ show x)

-- Indexed modification
list & itraversed %@~ (\i x -> i + x)

-- Filter by index
list & itraversed . indices even %~ (*2)

-- Index-aware folding
list ^@.. itraversed . filtered (> 5)  -- [(index, value)]
```

## Prisms (Sum Types)

```haskell
-- Match constructor
value ^? _Just
value ^? _Left
value ^? _Right
value ^? _Nothing  -- always Nothing or Just ()

-- Review (construct)
_Just # 42  -- Just 42
_Left # "error"  -- Left "error"

-- Modify if matches
value & _Just %~ (+1)

-- Try multiple prisms
value ^? _This `orOf` _That
```

## At vs Ix

```haskell
-- at: for insertable containers (returns Maybe)
map ^. at "key"           -- Maybe value
map & at "key" .~ Just v  -- insert
map & at "key" .~ Nothing -- delete

-- ix: for indexed containers (no-op if missing)
map ^? ix "key"           -- Maybe value
map & ix "key" .~ v       -- only updates if exists
list & ix 3 .~ v          -- only updates if index exists
```

## Working with Functions

```haskell
-- Apply function inside lens
record & field %~ reverse
record & field %~ map toUpper

-- Lens into function result
(\x -> x + 1) ^. re _Wrapped'  -- if using newtype

-- Modify with monadic action
record & field %%~ readFile  -- IO-based modification
```

## State Monad Patterns

```haskell
-- Zoom into state
zoom field $ modify (+1)

-- Use in State monad
field <%= (+1)  -- modify and return old value
field <<%~ (+1) -- modify and return old value
field <<.~ x    -- set and return old value
field <%~ (+1)  -- modify and return new value
field <.~ x     -- set and return new value

-- Get within State
use field
```

## Working with JSON (Aeson)

```haskell
-- Access JSON fields
json ^? key "field1" . key "field2" . _String
json ^? key "array" . nth 0

-- Deep JSON access
json ^? key "a" . key "b" . key "c" . _Number

-- Modify JSON
json & key "field" . _String %~ map toUpper
```

## Iso Patterns

```haskell
-- Reverse an Iso
from isoLens

-- Non (Nothing ≅ default value)
Nothing ^. non 0  -- 0
Just 5 ^. non 0   -- 5

-- Reversed
"hello" ^. reversed  -- "olleh"

-- Strict/Lazy conversion
lazyText ^. strict
strictText ^. lazy

-- Curried/Uncurried
(\(a,b) -> a + b) ^. curried  -- \a b -> a + b
(\a b -> a + b) ^. uncurried  -- \(a,b) -> a + b
```

## Existential Checks

```haskell
-- Check if traversal has any targets
has traverse list  -- is list non-empty?
has _Just maybeval -- is Just?
has (ix 3) list    -- does index 3 exist?

-- hasn't (negation)
hasn't _Nothing maybeval  -- is Just?
```

## Setting Multiple Fields

```haskell
-- Set multiple with same value
record & field1 .~ x & field2 .~ x

-- Set different values
record & field1 .~ x & field2 .~ y

-- Parallel modification
record & field1 %~ (+1) & field2 %~ (*2)
```

## Operators Summary

```haskell
-- Viewing
^.   -- view
^?   -- preview (safe, returns Maybe)
^?!  -- unsafe preview (errors on Nothing)
^..  -- toListOf
^@.. -- indexed toListOf

-- Setting
.~   -- set
?~   -- set to Just (for at/non)
.=   -- set in State monad
?=   -- set to Just in State monad

-- Modifying
%~   -- over (modify)
%%~  -- overA (applicative modify)
<<%~ -- modify, return old value
<%~  -- modify, return new value
%=   -- modify in State monad
<<%=, <%=, etc. -- State variants

-- Operators on Maybe/At
?~   -- set to Just value

-- Combining
<>~  -- append with Monoid
<>   -- combine results
```

## Lens Combinators

```haskell
-- taking: limit number of targets
list ^.. taking 3 traverse

-- dropping: skip first n targets
list ^.. dropping 2 traverse

-- takingWhile/droppingWhile
list ^.. takingWhile (< 5) traverse

-- filtered: only targets matching predicate
list ^.. traverse . filtered even

-- filteredBy: filter by another optic
records ^.. traverse . filteredBy (field . only "target")

-- worded: words in String
text ^.. worded
"hello world" & worded %~ reverse  -- "olleh dlrow"

-- lined: lines in String  
text ^.. lined

-- beside: apply two optics in parallel
(a, b) & beside _1 _2 %~ bimap (+1) (*2)
```

## Template Haskell Generation

```haskell
-- Generate lenses for all fields
makeLenses ''MyType      -- generates: fieldName

-- Generate lenses with underscore prefix
makeFields ''MyType      -- generates: field (if _field exists)

-- Generate prisms for constructors
makePrisms ''MySum       -- generates: _ConstructorName

-- Generate classy lenses
makeClassy ''MyType      -- generates: HasMyType class
```

## Common Gotchas & Tips

```haskell
-- Don't confuse at and ix
map ^. at "key"   -- Maybe value (always succeeds)
map ^? ix "key"   -- Maybe value (fails if missing for preview)

-- Use & for chaining (left-to-right)
record & field1 .~ x & field2 .~ y

-- Use . for composition (right-to-left)
record ^. field1 . field2

-- Traversals can have 0, 1, or many targets
emptyList ^.. traverse      -- []
singleton ^.. traverse      -- [x]
multiList ^.. traverse      -- [x, y, z]

-- Use partsOf for atomic list modification
"hello" & partsOf (traverse . filtered isVowel) .~ "aeiou"

-- sumOf/productOf for numeric folds
sumOf (traverse . field) records
```

## Type-Changing Updates

```haskell
-- Polymorphic update (changing types)
Just "hello" & _Just %~ length  -- Just 5 :: Maybe Int

-- Mapped (for Functors)
Just "hi" & mapped %~ length    -- Just 2
["a", "bb"] & mapped %~ length  -- [1, 2]

-- Traversed (for Traversables)
["a", "bb"] & traversed %~ length
```

## Performance Patterns

```haskell
-- Use (^.) for single field access (fast)
record ^. field

-- Use (^..) for multiple results (allocates list)
records ^.. traverse . field

-- Use toListOf explicitly when clearer
toListOf (traverse . field) records

-- Avoid repeated traversals
-- Bad: record ^. f1 + record ^. f2 + record ^. f3
-- Good: let r = record in r ^. f1 + r ^. f2 + r ^. f3
```

This covers the vast majority of common lens patterns you'll encounter in practice!
