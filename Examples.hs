{-# LANGUAGE OverloadedStrings #-}

module Test where

import STL
import STL.Check
import STL.Subsumption
import STL.DSL
import STL.Pretty hiding (list)

----------------------------------------------------------------------
-- Utils

(<:) :: Type -> Type -> IO ()
(<:) sub sup =
  let (res, state) =
        inferKindClosed sub == inferKindClosed sup `seq`
        runSubsumption (sub `subsumedBy` sup)
  in putDocLn $ vsep
     [ cpretty sub
     , indent 2 "<:"
     , cpretty sup
     , mempty
     , either cpretty (\_ -> "OK") $ res
     , "State:"
     , indent 2 $ cpretty state
     ]
infix 0 <:

(>:) :: Type -> Type -> IO ()
(>:) sup sub = sub <: sup

q :: Type -> IO ()
q ty =
  let k = inferKindClosed ty
      ty' = k `seq` normaliseClosed ty
      k' = inferKindClosed ty'
  in putDocLn $ vsep
     [ cpretty ty
     , "~~~>"
     , cpretty ty'
     , ":" <+> cpretty k' <+>
         if k /= k' then "/=" <+> cpretty k <+> "!!!" else mempty
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

----------------------------------------------------------------------

recOdd :: Type
recOdd =
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

recEven :: Type
recEven =
  Mu "Even" $
    Variant $
      Extend "EZero" Present Unit $
      Extend "ESucc" Present
        (Mu (Var "Odd") $
           Variant $
             Extend "OZero" Present Unit $
             Extend "OSucc" Present (Ref "Even") $
             Nil) $
        Nil

evenRecOdd :: Type
evenRecOdd =
  Variant $
    Extend "EZero" Present Unit $
    Extend "ESucc" Present recOdd $
    Nil

checkRecOddEven :: IO ()
checkRecOddEven = do
  evenRecOdd <: recEven
  evenRecOdd >: recEven
