# Structurally Typed interface description Language

An interface description language based on higher-rank and row-type
polymorphic structural type system.

Example schema:

```
-- Records:
type Color =
  { r : Int
  , g : Int
  , b : Int
  }

-- * `Int`, `Float`, and `String` are base types.

-- Variants with no payload (enums):
type Font =
  | SansSerif
  | Serif
  | Monospaced

-- Variants with payloads (and recursion):
type Drawing =
  | Circle    : { radius : Float, color : Color }
  | Square    : { side : Float, color : Color }
  | Label     : { caption : String, color : Color }
  | Scale     : { x : Float, y : Float }
  | Rotate    : { radians : Float }
  | Translate : { x : Float, y : Float }
  | Compose   : { background : Drawing, foreground : Drawing }

-- API entry point:
provide
  { canvasSize  : Drawing -> { width : Float, height : Float }
  , toGrayscale : Drawing -> Drawing
  , merge       : List Drawing -> Drawing
  }

-- * `a -> b` is type of functions with argument of type `a` and result of type `b`.
-- * `List` is parametrised type of lists. `List Int` means list of integers.
```

## Key features

* Structural types (as opposed to nominal types) mean that names do
  not matter: `type Foo = Bar` creates an alias `Foo` for type `Bar`;
  only the type structure matters.
* Order of record fields does not matter: `{foo : Int, bar : Bool}` *is*
  equivalent to `{bar : Bool, foo : Int}`; but the field names matter:
  `{bar : Int}` *is not* equivalent to `{baz : Int}`.
* Schema evolution is based on subtyping: if a new version of the type
  is a subtype of the old version, the change is not breaking; if it
  is not, then the change *is* breaking.
* If the schema of the service is a subtype of the schema of the
  client, the service and the client are guaranteed to be
  API-compatible (at least, in the communicated data structures).
* It is always possible to check programmatically if two schemas are
  compatible (and it is cheap to do so): old version vs. new version
  during schema refactoring or modification, updated service schema
  vs. known clients schemas during builds on CI, in run-time on
  establishing a connection between peers.
* Parametrised types, recursive types, and polymorphic function types
  *are supported*, so it is no longer necessary to duplicate parts of
  your interface just because of the limitations of the language.

## What is missing

* A proper name for the language.
* Comprehensible error messages from the subsumption checker.
* Module system with exports, imports, etc.
* User-defined foreign types.
* Bindings generator (for Haskell at least).
* Type term serialisation (with sharing).
* Run-time subsumption checker and type negotiation machinery.
