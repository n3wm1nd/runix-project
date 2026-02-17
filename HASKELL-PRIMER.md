# Haskell Primer for Runix

A quick orientation for developers coming from TypeScript, Python, or similar languages.
This is not a Haskell tutorial — just enough to read and navigate runix code.

## Module Structure

Every Haskell file is a module. At the top you'll see:

```haskell
{-# LANGUAGE GADTs #-}           -- language extensions (see appendix)
{-# LANGUAGE TypeApplications #-}

module Runix.Logging where        -- module declaration

import Data.Text (Text)           -- import specific names
import qualified Data.Map as Map  -- import under a prefix (Map.lookup, Map.insert, etc.)
```

`import qualified` means you must use the prefix: `Map.lookup` instead of
just `lookup`. This avoids name collisions (many modules export `lookup`,
`map`, etc.). The `as` part is just a shorter alias.

## Function Application

No parentheses needed. Arguments are separated by spaces:

```haskell
-- Python: greet("hello", "world")
-- Haskell:
greet "hello" "world"
```

Parentheses are only used for grouping, not for calling:

```haskell
-- Python: print(len(xs))
-- Haskell:
print (length xs)
```

Two operators show up constantly to avoid parentheses:

**`$` — "apply everything to the right":**

```haskell
-- These are the same:
print (reverse (sort xs))
print $ reverse $ sort xs
```

`$` is just function application but with low precedence, so everything to
its right gets evaluated first. It's a way to avoid nested parentheses.

**`.` — "compose functions" (pipe them together):**

```haskell
-- These are the same:
print (reverse (sort xs))
(print . reverse . sort) xs
```

`.` creates a new function by chaining others. Think of it as building a
pipeline (like `|` in a shell, but right-to-left): `print . reverse . sort`
means "sort, then reverse, then print." You'll see `.` a lot in interpreter
chains (more on that later).

One more: **`<>` is string/list concatenation** (like `+` in Python or `+` in
JS for strings). You'll see it everywhere: `"hello" <> " " <> "world"`.

## Do-Notation

Haskell code that performs effects uses `do` blocks, which look a lot like
imperative code. There are two kinds of bindings:

```haskell
myFunction = do
    let name = "hello"        -- pure assignment, no effects involved
    contents <- readFile path  -- effectful: "pull out" the result
    let upper = toUpper contents
    writeFile path upper
```

`<-` runs something that has an effect (reading a file, making an HTTP call,
etc.) and gives you the result. `let` is for plain values that don't involve
any effects — just computation.

Think of `<-` as `await` in async/await: it "unwraps" the effectful result
so you can use the plain value.

### `where` — Definitions After the Fact

`where` is the flip side of `let`. Instead of defining things before you use
them, you define them at the bottom:

```haskell
loggingIO = interpret $ \v -> do
    case v of
        Log level _ m -> embed $ putStrLn $ prefix level <> T.unpack m
  where
    prefix Info    = "info: "
    prefix Warning = "warn: "
    prefix Error   = " err: "
```

`where` and `let` are mostly interchangeable — use whichever reads better.
`where` is nice when you want the main logic up top and helper definitions
tucked away underneath. Both are just scoped aliases for values or functions.

### Pattern Matching

Instead of `if`/`else` chains, Haskell uses pattern matching to branch on
the shape of data. You'll see it in three forms:

**`case ... of` — inline branching:**

```haskell
case result of
    Left err  -> handleError err
    Right val -> useValue val
```

**`\case` — the same thing, but as a lambda (needs `LambdaCase`):**

```haskell
-- Very common in interpreters:
interpret $ \case
    GetConfig -> pure config
    Log level msg -> embed $ putStrLn msg
```

**Function equations — matching directly in the definition:**

```haskell
prefix Info    = "info: "
prefix Warning = "warn: "
prefix Error   = " err: "
```

All three forms do the same thing: check which constructor (or value) you
have and pick the matching branch. The compiler will warn you if you miss
a case.

## Reading Type Signatures

Function types use `->` to separate arguments and the return type:

```haskell
length :: [a] -> Int            -- takes a list, returns an Int
lookup :: String -> Map -> Maybe Value  -- takes two args, returns a Maybe
```

There's no distinction between "the last arrow" and the others — every `->`
just means "takes this, returns that." A function with three arguments is
really a chain: `a -> b -> c -> d` means "takes an `a`, returns a function
that takes a `b`, returns a function that takes a `c`, returns a `d`." In
practice you don't think about this — you just call it with three arguments.

Some common types you'll see:

| Type | What it is |
|---|---|
| `Text` | A string (the preferred string type in runix) |
| `[a]` | A list of `a` (e.g., `[String]` is a list of strings) |
| `(a, b)` | A tuple — a fixed-size pair of values |
| `Maybe a` | Either `Just value` or `Nothing` — like nullable/optional |
| `Either a b` | Either `Left error` or `Right value` — for results that can fail |
| `()` | Unit — the "void" type, means "returns nothing useful" |

## Types and Type Variables

Types start with an uppercase letter (`String`, `Int`, `Bool`). Type
variables — the generic/placeholder ones — start with lowercase (`a`, `r`,
`project`). The `::` operator means "has type":

```haskell
-- Concrete type: always a list of strings
names :: [String]

-- Generic: a list of anything
reverse :: [a] -> [a]
```

The type variable `a` here is what other languages call generics:

- **TypeScript:** `reverse<T>(list: T[]): T[]`
- **C# / Java:** `List<T> Reverse<T>(List<T> list)`
- **Python:** `def reverse(list: list[T]) -> list[T]`

Haskell just uses lowercase letters instead of angle brackets, and the
compiler infers them from usage.

### A Note on Data Types

Before getting into typeclasses, a quick mental model for Haskell data types.
**Types in Haskell are not classes, and values are not objects.** There is no
inheritance, no methods attached to types, no `this`/`self`. The closest
mental model is a **C# struct** or a **TypeScript plain object with a
type** — just data, no behavior baked in:

```haskell
data CmdOutput = CmdOutput
  { exitCode :: Int
  , stdout :: Text
  , stderr :: Text
  } deriving (Show, Eq)
```

This defines a type `CmdOutput` with three fields. `deriving (Show, Eq)`
auto-generates the ability to print it and compare it for equality — but
those are typeclass instances (see below), not methods on the type itself.
Functions that operate on `CmdOutput` are defined separately, not inside
the type.

You'll also see `newtype` — it's like `data` but restricted to exactly one
field. It's a zero-cost wrapper that creates a distinct type:

```haskell
newtype SystemPrompt = SystemPrompt Text
newtype UserPrompt = UserPrompt Text
```

Both wrap `Text`, but the compiler treats them as different types — you can't
accidentally pass a `UserPrompt` where a `SystemPrompt` is expected. There's
no runtime overhead; it's purely a compile-time distinction.

### Typeclasses: Interfaces, Not Inheritance

A typeclass defines a set of functions that a type must provide — like an
interface in TypeScript/C#/Java, or a protocol in Python. But crucially,
they're defined *separately* from the type, and a type can gain new
typeclass instances at any time without modifying its definition.

```haskell
-- Define an interface: "things that have a filesystem path"
class HasProjectPath project where
  getProjectPath :: project -> FilePath

-- Any type can implement it:
instance HasProjectPath FilePath where
  getProjectPath = id   -- a FilePath already is a path, just return it
```

Compared to other languages:

- **TypeScript:** `interface HasProjectPath { getProjectPath(): string }`
- **C#:** `interface IHasProjectPath { string GetProjectPath(); }`
- **Python:** `class HasProjectPath(Protocol): def get_project_path(self) -> str: ...`

The key difference: in those languages, the type itself declares that it
implements the interface. In Haskell, the `instance` is declared separately —
you can add instances for types you didn't even define.

### Typeclasses as Markers

Some typeclasses have no methods at all. They're pure markers — they just
say "this type has this property" so the compiler can check it:

```haskell
class SupportsStreaming a
class HasTools m where ...
```

A function can then require these as constraints:

```haskell
streamingQuery :: (SupportsStreaming model, HasTools model) => ...
```

This won't compile unless `model` is a type that has been declared to support
both streaming and tools. It's compile-time capability checking — the
equivalent of checking interface implementation, but at the type level with
no runtime cost.

### Constraints

When you see `=>` in a type signature, everything to the left is a constraint.
Constraints restrict what a type variable can be:

```haskell
sort :: Ord a => [a] -> [a]
```

This says: `sort` works on a list of any type `a`, as long as `a` has an
`Ord` instance (i.e., supports comparison). Think of it like:

- **TypeScript:** `sort<T extends Comparable>(list: T[]): T[]`
- **C#:** `List<T> Sort<T>(List<T> list) where T : IComparable`
- **Python:** `def sort(list: list[T]) -> list[T]` where `T` is bound by a `Protocol`

You can stack multiple constraints:

```haskell
showSorted :: (Ord a, Show a) => [a] -> String
```

This requires `a` to be both orderable *and* printable. In Polysemy code,
effect constraints follow the same pattern — `Member Logging r` is just
a constraint like `Ord a`, saying "the effect row `r` must include `Logging`."

## Effects in Runix (Polysemy)

Runix uses an effect system called Polysemy. The core idea: instead of a
function directly doing I/O, it *declares* what capabilities it needs, and
a separate interpreter decides how to fulfill them.

### Defining an Effect

Effects are defined as a GADT (a data type where each constructor can have
a different return type). Each constructor represents one operation:

```haskell
-- From Runix.Logging (simplified):
data Logging (m :: Type -> Type) a where
    Log :: Level -> Text -> Logging m ()
```

Read this as: "The `Logging` effect has one operation called `Log`, which
takes a level and a text message, and returns nothing (`()`)."

Each constructor is essentially a message — it *describes* what you want to
do, carrying all the information an interpreter will need. `Log` doesn't
actually print anything; it just says "I want to log this."

On top of this single operation, you can build convenience functions:

```haskell
info :: Member Logging r => Text -> Sem r ()
info t = log Info t

warning :: Member Logging r => Text -> Sem r ()
warning t = log Warning t
```

From the caller's perspective, `info`, `warning`, and `log` are all just
"functions that need `Logging`." The effect constraint is the same regardless
of which one you call.

A more interesting example with a type parameter:

```haskell
-- From Runix.Cmd:
data Cmd (command :: Symbol) (m :: Type -> Type) a where
    CmdExecWithStdin :: FilePath -> [String] -> ByteString -> Cmd command m CmdOutput
```

Here `command` is a type-level string (more on that below) that tags which
command this effect is allowed to run.

### Effect Functions

The GADT constructors are the raw operations, but you typically use
convenience functions that wrap them. These come in two flavors:

**Auto-generated with `makeSem`:**

```haskell
makeSem ''Logging  -- generates: log :: Member Logging r => Level -> Text -> Sem r ()
```

`makeSem` looks at the GADT constructors and generates a function for each
one. The generated function has the same name (lowercased) and includes the
right effect constraint.

**Hand-written with `send`:**

```haskell
-- From Runix.Cmd:
call :: forall command r. Member (Cmd command) r => [String] -> Sem r CmdOutput
call args = send @(Cmd command) (CmdExecWithStdin "." args BS.empty)
```

Hand-written functions can provide a nicer API (e.g., defaulting the working
directory), combine multiple operations, or handle error conversion. The
mapping between GADT constructors and user-facing functions doesn't have to
be one-to-one.

### Effect Constraints: What "Needs to be Available" Means

Look at this function signature:

```haskell
listFiles :: forall project r. Members [FileSystem project, Fail] r => FilePath -> Sem r [FilePath]
```

The `Members [FileSystem project, Fail] r` part says: "this function requires
`FileSystem project` and `Fail` to be available in the effect row `r`."

This is the key insight: **if your function uses an effect, it must declare
that effect in its constraints.** The constraint is exactly the list of
capabilities your function needs. Anyone calling your function automatically
inherits those requirements — their signature must include the same effects
(or a superset).

```haskell
-- This function needs Logging and Fail
myHelper :: Members [Logging, Fail] r => Sem r ()

-- This function calls myHelper, so it also needs Logging and Fail,
-- plus FileSystem for its own work
myFunction :: Members [FileSystem project, Logging, Fail] r => Sem r ()
myFunction = do
    files <- listFiles @project "src"
    info "found some files"
    myHelper
```

`Member` (singular) is for when you need just one effect:

```haskell
getConfig :: Member (Config c) r => Sem r c
```

### Interpreters: Providing the Implementation

An interpreter takes a program that uses an effect and runs it, removing
that effect from the stack. The type signature tells the story:

```haskell
-- From Runix.Config:
runConfig :: c -> Sem (Config c : r) a -> Sem r a
runConfig con = interpret $ \case
    GetConfig -> pure con
```

Read the type: it takes a `Sem` with `Config c` on the stack and returns a
`Sem` *without* it. The interpreter "handles" the effect by providing a
concrete implementation for each constructor.

This feels backwards at first — the interpreter *adds* a capability by
*removing* it from the type. But it makes sense: the code *inside* needs the
effect (so it's in the input type), and after the interpreter handles it,
the effect is satisfied (so it's gone from the output type).

Interpreters compose by chaining. Here's a real example from the codebase
(`Runner.hs`):

```haskell
runWithEffects action =
    runM                    -- final step: run the remaining Embed IO
    . runError              -- handle errors
    . loggingIO             -- handle Logging (by printing to stdout)
    . failLog               -- handle Fail (by logging + throwing)
    . cancelNoop            -- handle Cancellation (as no-op)
    . httpIO                -- handle HTTP
    . cmdsIO                -- handle Cmds
    . bashIO                -- handle Bash
    . filesystemIO          -- handle FileSystem
    $ action
```

Each interpreter peels off one effect. By the end, all effects are handled.

## Explicit Type Variables with `forall`

When a function has type variables that you need to reference in the body
(or that callers need to specify explicitly), you declare them with `forall`:

```haskell
readFile :: forall project r. Members [FileSystemRead project, Fail] r => FilePath -> Sem r ByteString
readFile p = send @(FileSystemRead project) (ReadFile p)
--                  ^^^^^^^^^^^^^^^^^^^^
--                  we can reference `project` here because of the forall
```

Without `forall`, the type variable exists but you can't refer to it in the
function body. The `forall` brings it into scope.

Notice the naming convention at work: `project` and `r` are lowercase, so
they're type variables (to be filled in later). `FileSystemRead`, `Fail`,
`ByteString` are uppercase — concrete types.

## TypeApplications: Setting Type Variables Explicitly

Sometimes the compiler can't figure out which type you mean for a type
variable. TypeApplications let you specify it with `@`:

```haskell
-- readFile is generic over `project`, so we tell it which project:
contents <- readFile @MyProject "config.yaml"
```

The `@` sets the first type variable from the `forall`. In the example above,
`readFile` has `forall project r.`, so `@MyProject` fills in `project`. The
compiler usually figures out `r` on its own from context.

This is why the order of type variables in `forall` matters — put the ones
the caller is most likely to specify first, just like you'd put the most
important function arguments first. `forall project r.` means `project` can
be set with a single `@`, while `r` is almost always inferred.

You'll see this pattern constantly in runix for parameterized effects:

```haskell
-- From Runix.Cmd:
call :: forall command r. Member (Cmd command) r => [String] -> Sem r CmdOutput

-- Usage:
output <- call @"git" ["status"]
```

Sometimes the compiler can infer the type argument and you don't need `@` at
all. When it can't, it will tell you with an error about ambiguous types.

## Types as Markers (Phantom Types)

Sometimes a type's only job is to be a label — it doesn't carry any data at
runtime. These are called phantom types:

```haskell
-- The `project` parameter in FileSystem doesn't appear in the data the
-- constructors carry. It's purely a compile-time tag:
data FileSystem project (m :: Type -> Type) a where
    GetFileSystem :: FileSystem project m project
    ListFiles :: FilePath -> FileSystem project m (Either String [FilePath])
```

This lets you have multiple file systems on the same effect stack, distinguished
by type:

```haskell
-- These types exist only as labels — they don't need to carry any data:
data MainRepo
data DepRepo

myFunc :: Members [FileSystemRead MainRepo, FileSystemRead DepRepo] r => Sem r ()
myFunc = do
    main <- readFile @MainRepo "config.yaml"
    dep  <- readFile @DepRepo  "lib/code.hs"
```

`MainRepo` and `DepRepo` have no constructors — you can never create a value
of these types, and you don't need to. They exist purely at the type level to
tell the compiler which file system you mean.

Without the phantom type, you'd have no way to tell the two file systems apart
at the type level — they'd both just be `FileSystemRead`.

### Type-Level Strings

Haskell can use string literals as types. These fill the same marker role
as phantom types above, but you don't need to define a new type first:

```haskell
-- From Runix.Cmd — the command name is a type-level string:
data Cmd (command :: Symbol) (m :: Type -> Type) a where
    CmdExecWithStdin :: FilePath -> [String] -> ByteString -> Cmd command m CmdOutput

-- Now different commands are different types:
gitStatus  :: Member (Cmd "git") r    => Sem r CmdOutput
cabalBuild :: Member (Cmd "cabal") r  => Sem r CmdOutput
```

`Member (Cmd "git") r` and `Member (Cmd "cabal") r` are completely different
constraints. A function that requires `Cmd "git"` cannot accidentally run
`cabal` — the type system prevents it.

The `Symbol` kind annotation means "this is a type-level string." At runtime,
you can recover the actual string value with `symbolVal` if needed (for
example, to know which binary to execute), but its primary purpose is
compile-time safety.

You could achieve the same thing with phantom types (`data Git` / `data Cabal`)
instead of strings. Type-level strings are just more convenient when the label
maps directly to a runtime string you'll need anyway (like a command name).

## Summary of Key Patterns

| Pattern | What it means |
|---|---|
| `f x y` | Call `f` with arguments `x` and `y` |
| `f $ g x` | Same as `f (g x)` — avoids parentheses |
| `f . g` | Compose: `(f . g) x` is `f (g x)` |
| `a -> b` | Function type: takes `a`, returns `b` |
| `x <- action` | Run `action`, bind the result to `x` |
| `let x = expr` | Pure binding, no effects |
| `where x = expr` | Same as `let`, but defined after usage |
| `case x of` / `\case` | Pattern match on a value |
| `x :: Type` | Type annotation — "`x` has type `Type`" |
| `data Foo = Foo { ... }` | Define a data type (like a struct) |
| `newtype Foo = Foo Text` | Zero-cost type wrapper |
| `class Foo a where` | Define a typeclass (like an interface) |
| `instance Foo Bar where` | `Bar` implements the `Foo` interface |
| `Ord a =>` | Constraint: `a` must have an `Ord` instance |
| `Member Effect r =>` | This function requires `Effect` |
| `Members [A, B, C] r =>` | Requires all three effects |
| `Sem r a` | A computation using effects in `r`, returning `a` |
| `forall a r.` | Bring type variables into scope |
| `@MyType` | Explicitly set a type variable |
| `Sem (Effect : r) a -> Sem r a` | An interpreter that handles `Effect` |

## Appendix: Language Extensions

Haskell modules in runix start with a block of `{-# LANGUAGE ... #-}` lines.
These are language extensions — they opt in to features beyond the base
language. The compiler will usually tell you if you're missing one. Here's
what the common ones do and why they're there:

**The effect system core** — nearly every module that defines or uses effects
needs these:

| Extension | What it enables |
|---|---|
| `GADTs` | Lets each constructor in a data type have a different return type. This is how effects are defined — `Log` returns `Logging m ()`, `ReadFile` returns `FileSystemRead project m (Either String ByteString)`, etc. |
| `DataKinds` | Lets values be used as types. This is what makes `"git"` work as a type in `Cmd "git"`, and why effect rows like `'[Logging, Fail]` can be type-level lists. |
| `TypeOperators` | Allows operators in types. The `:` in `Sem (Logging : r) a` is a type operator (cons for type-level lists). Without this, you'd have to write it differently. |
| `KindSignatures` | Lets you annotate the "type of a type." The `(m :: Type -> Type)` in effect GADTs says "`m` is a type constructor that takes one type and returns another." |
| `FlexibleContexts` | Allows complex things in constraints, like `Member (Cmd "git") r` instead of just simple `Ord a` style constraints. Polysemy's `Member` constraints need this. |

**Type variable plumbing** — for `forall`, `@TypeApplication`, and passing
types around:

| Extension | What it enables |
|---|---|
| `ScopedTypeVariables` | Makes `forall` actually bring type variables into scope in the function body, so you can reference them with `@`. Without this, `forall` is purely decorative. |
| `TypeApplications` | The `@Type` syntax for explicitly setting type variables: `readFile @MyProject`, `call @"git"`. |
| `AllowAmbiguousTypes` | Lets you define functions where a type variable only appears in constraints, not in the arguments or return type. Functions like `getFileSystem` need this — the `project` type can only be specified via `@`. |
| `RankNTypes` | Allows `forall` to appear inside a type, not just at the top. Used for interpreter types like `forall r. Members [...] r => Sem r a` nested inside a record. |

**Template Haskell and code generation:**

| Extension | What it enables |
|---|---|
| `TemplateHaskell` | Compile-time code generation. `makeSem ''Logging` uses this to inspect the `Logging` GADT and auto-generate the `log` function. The `''` is Template Haskell syntax for referring to a type by name. |

**Syntactic convenience:**

| Extension | What it enables |
|---|---|
| `LambdaCase` | Shorthand for `\x -> case x of`. The `\case` in interpreters (`interpret $ \case ...`) uses this. |
| `OverloadedStrings` | Lets string literals like `"hello"` work as `Text`, `ByteString`, etc., not just `String`. |
| `OverloadedRecordDot` | Lets you write `record.field` instead of `field record` to access record fields. |

**Instance flexibility** — the compiler is conservative by default about
typeclass instances; these loosen the rules:

| Extension | What it enables |
|---|---|
| `FlexibleInstances` | Allows typeclass instances for complex types like `FileSystem MyProject` instead of just simple type variables. |
| `MultiParamTypeClasses` | Allows typeclasses with multiple type parameters, like `class HasAuth provider model`. |
| `UndecidableInstances` | Tells the compiler "trust me, this instance resolution will terminate." Occasionally needed for advanced Polysemy patterns. |

A typical effect module will have most of the "core" and "type variable"
extensions enabled. You don't need to memorize them — just know that the
block at the top is unlocking the features described in this guide.

## Where to Go from Here

- **Polysemy**: the effect system — look at existing effects in `runix/src/Runix/` for patterns
- **GADTs**: how effects are defined — each constructor is an operation
- **TypeApplications**: the `@Type` syntax for specifying type parameters
- **Do-notation**: Haskell's way of writing sequential effectful code

You don't need to understand Monads, Functors, or category theory to work
with runix. The effect system handles the plumbing. Focus on: what effects
does my function need? What operations are available? How do I wire up the
interpreter?
