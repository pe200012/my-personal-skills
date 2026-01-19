
---
name: haskell-lens-best-practices
version: 1.0.0
description: Comprehensive guidelines for Haskell Lenses & Optics Best Practices.
author: pe200012
license: BSD-3-Clause
tags: [haskell, lenses, functional-programming, best-practices]
---

# Haskell Lenses & Optics Best Practices

## Purpose
Mastering the use of Haskell lenses and optics for querying, manipulating, and traversing immutable data structures. This skill covers library selection, common operators, best practices, and concrete recipes for everyday tasks.

## 1. Quick Cheatsheet: Operators

| Operator | Name | Mnemonic | Usage | Type |
|----------|------|----------|-------|------|
| `^.` | view | "view point" | `structure ^. lens` | Get a value |
| `.~` | set | "set to" | `structure & lens .~ newValue` | Set a value |
| `%~` | over | "mod over" | `structure & lens %~ function` | Modify a value |
| `^?` | preview | "question?" | `structure ^? prism` | Get Maybe value |
| `^..` | toListOf | "dot dot list" | `structure ^.. traversal` | Get List of values |
| `&` | apply | "block ref" | `x & f` (reverse application) | Chain updates |

## 2. Library Selection Guide

Choose the right library for your project's needs:

- **`lens` (Edward Kmett)**
  - **Pros**: The "batteries-included" standard. Massive ecosystem.
  - **Cons**: Heavy compilation weight. Large dependency footprint.
  - **Use When**: Building applications, data science scripts, or when you need everything (isomorphisms, weird instances).

- **`microlens`**
  - **Pros**: Lightweight, zero-dependency. API compatible with `lens`.
  - **Cons**: Smaller feature set (mostly lacking complex Isos and some instances).
  - **Use When**: **Writing libraries** (minimize downstream dependencies).

- **`optics`**
  - **Pros**: Abstract interface (better error messages!), explicit types (`Lens`, `Prism` instead of `forall f...`).
  - **Cons**: Different internal representation (not fully compatible with `lens` without adapters).
  - **Use When**: Starting a new application and you want readable type errors.

- **`generic-lens` / `generic-optics`**
  - **Pros**: No Template Haskell boilerplate (`makeLenses` not needed). Uses GHC Generics.
  - **Use When**: Prototyping or working with simple records where boilerplate is a blocker.

## 3. Best Practices & Anti-Patterns

### ✅ Do:
- **Use Template Haskell**: `makeLenses` is standard. Prefix fields with `_` (e.g., `_userName`).
- **Chain updates**: Use `&` to chain multiple updates clearly.
  ```haskell
  user & name .~ "Alice"
       & score %~ (+10)
  ```
- **Use `microlens` for libraries**: Avoid forcing `lens` on your users.
- **Learn the hierarchy**: `Iso` > `Lens` > `Prism` > `Traversal` > `Fold`. A Lens is a valid Traversal, but not vice versa.

### ❌ Don't:
- **Overuse symbolic operators**: If code gets unreadable, use named functions (`view`, `set`, `over`).
- **Expose lenses directly in public APIs**: Unless your library is *about* data manipulation. Consider exporting standard getters/setters or a dedicated `Lens` module.
- **Break Lens Laws**: Lenses must be well-behaved (Get-Put, Put-Get, Put-Put). Don't make a lens that modifies data unexpectedly when "getting".

## 4. Common Recipes

### Deeply Nested Updates
Modify a field deep inside a structure without unpacking everything.
```haskell
-- Define data
data Config = Config { _net :: Network }
data Network = Network { _port :: Int }
makeLenses ''Config
makeLenses ''Network

-- Update
updatePort :: Config -> Config
updatePort cfg = cfg & net . port .~ 8080
```

### Working with Maps (`At` and `Ix`)
- `at`: Access `Maybe v` (allows inserting/deleting).
- `ix`: Access `v` (traversal, only targets existing values).

```haskell
import Control.Lens

myMap :: Map String Int
myMap = fromList [("a", 1)]

-- Insert or Delete
insertB = myMap & at "b" .~ Just 2
deleteA = myMap & at "a" .~ Nothing

-- Modify only if exists
doubleA = myMap & ix "a" %~ (*2) -- No-op if "a" missing
```

### JSON Manipulation (`lens-aeson`)
Drill into JSON without defining types.
```haskell
import Data.Aeson.Lens
import Control.Lens

jsonBlob ^? key "users" . nth 0 . key "name" . _String
-- Result: Just "Alice"
```

### Enum/Sum Types with Prisms
Access or construct branches of a Sum type.
```haskell
data Response = Success String | Error Int
makePrisms ''Response

-- Construct
resp = _Success # "Ok"

-- Match/Preview
msg :: Maybe String
msg = resp ^? _Success
```

### Modifying Lists (`traversed`)
Apply a change to all elements in a list (or any Traversable).
```haskell
users :: [User]
activeUsers = users & traversed . isActive .~ True
```

## 5. Advanced Patterns (from "Optics by Example")

### Virtual Fields
Lenses that don't map 1:1 to stored fields but compute values on the fly.
- **Example**: A `fahrenheit` lens on a struct that stores `celsius`.
- **Benefit**: Decouples internal storage from external API.

### Self-Correcting Lenses
Lenses that maintain invariants during updates.
- **Example**: A `time` lens that clamps values between 0-24 hours.
- **Note**: This technically violates lens laws (Set-Get) but is practically useful.

### Classy Lenses
Using type classes (`HasLens`) to decouple code from concrete data structures.
- **Pattern**: `class HasName a where name :: Lens' a String`
- **Benefit**: Functions can operate on *any* type that has a `name` field.

### Polymorphic Optics
Optics that change the type of the container when the focus changes.
- **Type**: `Lens s t a b` (Input `s`, Output `t`, Focus `a`, New Focus `b`).
- **Use Case**: Changing `Box Int` to `Box String`.

## 6. Core Concepts Breakdown

- **Lens**: Focus on **one** piece of data inside a product (like a struct field).
- **Prism**: Focus on **one** branch of a sum type (like `Just` in `Maybe` or `Right` in `Either`).
- **Traversal**: Focus on **0 to N** pieces of data (like items in a List).
- **Iso**: Lossless conversion between types (e.g., `String` <-> `Text`).
- **Fold**: Read-only access to **0 to N** values (like a Getter, but for many values).
- **Getter**: Read-only access to **one** value (a function `s -> a`).
- **Setter**: Write-only access to **0 to N** values (a function `(a -> b) -> s -> t`).
- **Review**: Write-only access to construct a value (reverse of a Getter, used with Prisms).
- **Affine Traversal**: Focus on **0 or 1** piece of data (like a safe Lens that might miss).
- **Indexed Optic**: Any optic that also provides access to an index/key (`Int`, `Key`, etc.) along with the value.

## 7. Reference Examples
This skill includes a comprehensive suite of executable examples in the `examples/` directory:

| File | Concept | Source |
|------|---------|--------|
| `01_basics_and_setup.hs` | Basics (`^.`, `.~`, `%~`) | Standard Lib |
| `02_nested_structures.hs` | Deep Updates | Wire/Plutus |
| `03_traversals_and_containers.hs` | Lists & Maps | Calamity |
| `04_json_manipulation.hs` | `lens-aeson` | PostgREST |
| `05_sum_types_prisms.hs` | Sum Types (`makePrisms`) | Servant |
| `06_generic_lens.hs` | `generic-lens` (`#field`) | Ormolu |
| `07_optic_hierarchy_glassery.hs` | Optics Hierarchy | Glassery Blog |
| `08_virtual_fields.hs` | Computed Lenses | Optics By Example (Ch 3.5) |
| `09_self_correcting_lenses.hs` | Invariant Enforcement | Optics By Example (Ch 3.6) |
| `10_classy_lenses.hs` | Classy Lenses (`HasLens`) | Optics By Example (Ch 14) |
| `11_polymorphic_lenses.hs` | Type Changing (`Lens s t a b`) | Optics By Example (Ch 4) |
| `12_indexed_optics.hs` | Indexed Optics (`itraversed`) | Optics By Example (Ch 11) |
| `13_state_monad_zoom.hs` | State Monad (`zoom`, `use`) | Optics By Example (Ch 13) |
