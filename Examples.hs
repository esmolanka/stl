{-# LANGUAGE OverloadedStrings          #-}

module Test where

import STL
import STL.Check
import STL.Subsumption
import STL.DSL
import Data.Text.Prettyprint.Doc

recordSelect :: Type
recordSelect =
  Forall "a" Star $
    Record (Extend "foo" Present (Ref "a" 0) Nil) :~> Ref "a" 0

bool :: Type
bool =
  Variant $
    Extend "True" Present Unit $
    Extend "False" Present Unit $
    Nil

someRecord :: Type
someRecord =
  Record (Extend "foo" Present bool Nil)

pair :: Label -> Label -> Type
pair la lb =
  Lambda "a" Star $
    Lambda "b" Star $
      Record $
        Extend la Present (Ref "a" 0) $
        Extend lb Present (Ref "b" 0) $
        Nil

alt :: Label -> Label -> Type
alt la lb =
  Lambda "a" Star $
    Lambda "b" Star $
      Variant $
        Extend la Present (Ref "a" 0) $
        Extend lb Present (Ref "b" 0) $
        Nil

list :: Type
list =
  Lambda "el" Star $
    Mu "list" $
      alt "Nil" "Cons" :$
        Unit :$
        (pair "head" "tail" :$
          Ref "el" 0 :$
          Ref "list" 0)

----------------------------------------------------------------------

moduleA :: Type
moduleA =
  Forall "a" Star $
  Forall "b" Star $
    Record $
      Extend "to"   Present (Ref "a" 0 :~> Ref "b" 0) $
      Extend "from" Present (Ref "b" 0 :~> Ref "a" 0) $
      Nil

moduleB :: Type
moduleB =
  Record $
    Extend "to"   Present (Unit :~> Unit) $
    Extend "from" Present (Unit :~> Unit) $
    Nil

programA :: Program
programA =
  Let "List" [("el", Star)]
   (Variant $
      Extend "Nil"  Present Unit $
      Extend "Cons" Present (
        Record $
          Extend "head" Present (Ref "el" 0) $
          Extend "tail" Present (Global "List") $
          Nil) $
      Nil) $
  Return
   (Global "List" :$ (Global "List" :$ Unit))
