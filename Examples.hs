{-# LANGUAGE OverloadedStrings #-}

module Test where

import STL
import STL.Check
import STL.Subsumption
import STL.DSL
import Data.Text.Prettyprint.Doc

----------------------------------------------------------------------
-- Utils

(<:) :: Type -> Type -> Doc a
(<:) sub sup =
  let (res, state) =
        inferKindClosed sub == inferKindClosed sup `seq`
        runSubsumption (subsumes sub sup)
  in vsep
     [ pretty sub
     , indent 2 "<:"
     , pretty sup
     , mempty
     , either pretty (\_ -> "OK") $ res
     , "State:"
     , indent 2 $ pretty state
     ]
infix 0 <:

q :: Type -> Doc a
q ty =
  let k = inferKindClosed ty
      ty' = k `seq` normaliseClosed ty
      k' = inferKindClosed ty'
  in vsep
     [ pretty ty
     , "~~~>"
     , pretty ty'
     , ":" <+> pretty k' <+> if k /= k' then "/=" <+> pretty k <+> "!!!" else mempty
     ]

----------------------------------------------------------------------

bool :: Type
bool =
  Variant $
    Extend "True" Present Unit $
    Extend "False" Present Unit $
    Nil

pair :: Label -> Label -> Type
pair la lb =
  Lambda "a" Star $
    Lambda "b" Star $
      Record $
        Extend la Present (Ref "a") $
        Extend lb Present (Ref "b") $
        Nil

alt :: Label -> Label -> Type
alt la lb =
  Lambda "a" Star $
    Lambda "b" Star $
      Variant $
        Extend la Present (Ref "a") $
        Extend lb Present (Ref "b") $
        Nil

list :: Type
list =
  Lambda "el" Star $
    Mu "list" $
      alt "Nil" "Cons" :$
        Unit :$
        (pair "head" "tail" :$
          Ref "el" :$
          Ref "list")


----------------------------------------------------------------------

someRecord :: Type
someRecord =
  Record (Extend "foo" Present bool Nil)

recordSelect :: Type
recordSelect =
  Forall "a" Star $
    Record (Extend "foo" Present (Ref "a") Nil) :~> Ref "a"

----------------------------------------------------------------------

moduleA :: Type
moduleA =
  Forall "a" Star $
  Forall "b" Star $
    Record $
      Extend "to"   Present (Ref "a" :~> Ref "b") $
      Extend "from" Present (Ref "b" :~> Ref "a") $
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
          Extend "head" Present (Ref "el") $
          Extend "tail" Present (Global "List") $
          Nil) $
      Nil) $
  Return
   (Global "List" :$ (Global "List" :$ Unit))

mutuallyRecursive1 :: Type
mutuallyRecursive1 =
  Mu (Var "Odd") $
    Variant $
      Extend "OZero" Present Unit $
      Extend "OSucc" Present
        (Mu "Even" $
          Variant $
            Extend "EZero" Present Unit $
            Extend "ESucc" Present
              (Ref "Odd") $
            Nil) $
      Nil
