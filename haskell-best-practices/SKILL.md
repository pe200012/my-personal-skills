
---
name: haskell-best-practices
description: Comprehensive guidelines for Haskell development, covering type safety, error handling, performance, and project structure.
---

# Haskell Best Practices

## Overview

**Haskell best practices prioritize correctness via type safety, purity, and disciplined side-effect management.**

The philosophy is "Make Illegal States Unrepresentable." If a state is invalid in your domain logic, the type system should prevent it from existing. This shifts bugs from runtime exceptions to compile-time errors.

**REQUIRED BACKGROUND:** Functional programming fundamentals (Monads, Purity, Immutability), `superpowers:test-driven-development`.

## Core Guidelines

### 1. Type-Driven Design (Parse, Don't Validate)
Do not pass loose data (like `String` or `Int`) around and validate it deep in your call stack. Parse it at the boundary into a strict type that proves validity.

*   **Bad:** Passing `Int` representing an age and checking `age >= 0` in every function.
*   **Good:** Define `newtype Age = Age Int` with a smart constructor that only returns `Just Age` if valid.

### 2. Functional Core / Imperative Shell
Keep your business logic pure. Isolate `IO` (database, network, config reading) to the "edges" of your application.

*   **Pure Core:** Functions that take input and return output. deterministic, easy to test.
*   **Imperative Shell:** `Main.hs` or handler layers that gather data, call the core, and execute effects.

### 3. Error Handling Hierarchy
Choose the right tool for the job. Avoid runtime exceptions (`error`, `undefined`) in production code.

*   **`Maybe a`**: For optional values or simple failures where the "why" doesn't matter (e.g., finding item in list).
*   **`Either e a`**: For expected failures where the error reason matters (e.g., parsing, validation). Define a custom sum type for `e`.
*   **`MonadError` / Transformers**: For composing complex error handling stacks.
*   **Exceptions (IO)**: Reserve for truly exceptional, unrecoverable runtime states (Disk full, Network panic) typically handled in `IO`.

### 4. Performance & Data Structures
Haskell is lazy by default. Uncontrolled laziness leads to memory leaks ("space leaks").

*   **String vs Text:**
    *   `String` (Linked List of Char): **Avoid** for data. O(n) space/time overhead. Use only for simple code literals or legacy APIs.
    *   `Text` (Data.Text): Use for all human-readable text.
    *   `ByteString` (Data.ByteString): Use for binary data and network protocols.
*   **Strictness:**
    *   Use `strict` fields in data types (`data Foo = Foo !Int !Text`).
    *   Use `foldl'` (strict fold) instead of `foldl`.
*   **Collections:**
    *   Use `Vector` instead of `List` for large indexed collections.
    *   Use `Map` or `HashMap` for lookups.

### 5. Testing & Quality
*   **Property-Based Testing (QuickCheck/Hedgehog):** Test *properties* (e.g., "reversing a list twice equals the original") rather than just examples.
*   **Golden Tests:** Compare output against a known "golden" file for complex structures.
*   **Linting:** Use `HLint` to catch common styling and performance issues.

## Implementation Example

**Clean Module Pattern:**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-} -- Consider using Relude or RIO

module Domain.User 
    ( User         -- Export Type
    , mkUser       -- Export Smart Constructor
    , userName     -- Export Accessor
    ) where

import qualified Data.Text as T
import           Prelude

-- Internal representation
data User = User 
    { userId   :: !Int     -- Strict field (!)
    , userName :: !T.Text 
    } deriving (Eq, Show)

-- Smart Constructor - enforces invariants
mkUser :: Int -> T.Text -> Either T.Text User
mkUser uid name
    | T.null name = Left "Name cannot be empty"
    | uid < 0     = Left "ID cannot be negative"
    | otherwise   = Right $ User uid name
```

## Quick Reference

| Category | Best Practice | Rationale |
| --- | --- | --- |
| **Safety** | No `head`, `tail`, `!!` | Partial functions cause runtime crashes. Use pattern matching or safe alternatives. |
| **Types** | `newtype` | Zero-cost abstractions to prevent mixing up `UserId` and `OrderId` (both `Int`). |
| **Loops** | Recursion vs Folds | Prefer `map`, `filter`, `foldl'` over manual recursion for readability and safety. |
| **Imports** | `import qualified` | Avoids namespace pollution. `import qualified Data.Text as T`. |
| **Config** | `DeriveGeneric`, `FromJSON` | Use Aeson for easy JSON parsing instead of manual parsing. |

## Common Pitfalls & Fixes

### The "Partial Function" Trap
*   **Mistake:** Using `head xs` to get the first element.
*   **Risk:** Crashes if `xs` is empty.
*   **Fix:** Use `Data.List.NonEmpty` if the list *cannot* be empty by design, or pattern match `case xs of [] -> ...`.

### The "Space Leak" Trap
*   **Mistake:** Accumulating a large thunk in a recursive function or `foldl`.
*   **Risk:** Exhausting memory (Stack/Heap overflow) with unevaluated expressions.
*   **Fix:** Use strict data fields (`!`) and strict functions (`foldl'`, `modify'`).

### The "Boolean Blindness" Trap
*   **Mistake:** Returning `Bool` to indicate success/failure.
*   **Risk:** Caller forgets what `True` means (Success? Failure? Found? Not Found?).
*   **Fix:** Return `Maybe`, `Either`, or a custom Sum Type (`data Result = Success | InvalidInput`).

## Rationalization Table

| Rationalization | Reality |
| --- | --- |
| "I'll strict-ify this data later." | Space leaks are hard to debug later. Make data strict by default. |
| "Exceptions are easier than Either." | Exceptions are invisible in the type signature. `Either` forces the caller to handle the error. |
| "I don't need QuickCheck, unit tests are enough." | You will miss edge cases. Generators find bugs you can't imagine. |
| "This list is never empty." | Prove it with `NonEmpty`. Constraints documents assumptions. |

## Red Flags - STOP and Refactor

*   Usage of `head`, `tail`, `init`, `last`, `!!` or `read` (unsafe).
*   Nested `IO` calls inside business logic functions.
*   Functions taking more than 3 primitive arguments (use a Record/Config object).
*   Manually parsing strings instead of using parser combinators (Parsec/Megaparsec) or Aeson.
*   "Stringly Typed" programming (passing data as Strings everywhere).

**Code that compiles should run without crashing. If it crashes, the types were too loose.**
