# Structurally Typed interface description Language

An interface description language with smoother schema evolution
experience that utilises row-types and higher rank polymorphism.

## Supported features

* Row-type polymorphic records and variants with presence types
* Higher-kinded types
* Higher-rank polymorphism
* Existential quantification
* Recursive and mutually recursive types with structural subsumption checking
* REPL and batch checking mode
* Emacs mode with highlighing and Flycheck

## What is still missing

* A proper name for the language
* Comprehensible error messages from the subsumption checker
* Module system with exports, imports, etc.
* Bindings generator (for Haskell at least)
* Type term serialisation (with sharing)
* Run-time subsumption checker and type negotiation machinery
* Everything else
