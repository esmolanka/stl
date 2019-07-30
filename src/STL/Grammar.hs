{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module STL.Grammar where

import Control.Category ((>>>))
import Data.Functor.Foldable (Fix(..))
import Data.Coerce
import Data.Char (isUpper)
import qualified Data.Text as T
import Data.Text (Text, isPrefixOf)
import Language.SexpGrammar
import Language.SexpGrammar.Generic

import STL.Types

fixGrammar
  :: Grammar Position (Sexp :- t) (f (Fix f) :- t)
  -> Grammar Position (Sexp :- t) (Fix f :- t)
fixGrammar g = g >>> iso coerce coerce

varGrammar :: Grammar Position (Sexp :- t) (Var :- t)
varGrammar = symbol >>> partialOsi parseVar coerce
  where
    parseVar :: Text -> Either Mismatch Var
    parseVar t =
      if t `elem` ["forall","mu","lambda","let","record","variant"] ||
         ":" `isPrefixOf` t
      then Left (unexpected t)
      else Right (Var t)

globalNameGrammar :: Grammar Position (Sexp :- t) (GlobalName :- t)
globalNameGrammar = symbol >>> partialOsi parseGlobalName coerce
  where
    parseGlobalName :: Text -> Either Mismatch GlobalName
    parseGlobalName t =
      case T.uncons t of
        Just (n, _) | isUpper n -> Right (GlobalName t)
        _ -> Left (unexpected t)

kindGrammar :: Grammar Position (Sexp :- t) (Kind :- t)
kindGrammar = match
  $ With (\star -> sym "Star" >>> star)
  $ With (\row  -> sym "Row"  >>> row)
  $ With (\pres -> sym "Presence" >>> pres)
  $ With (\arr  -> list (el (sym "->") >>> el kindGrammar >>> el kindGrammar) >>> arr)
  $ End

paramGrammar :: Grammar Position (Sexp :- t) ((Var, Kind) :- t)
paramGrammar =
  (varGrammar >>> push Star (== Star) (\_ -> expected "Star") >>> pair) <>
  (list (el varGrammar >>> el (sym ":") >>> el kindGrammar) >>> pair)

typeGrammar :: Grammar Position (Sexp :- t) (Type :- t)
typeGrammar = fixGrammar $ match
  $ With (\ref ->
            annotated "ref" $
            position >>>
            swap >>>
            bracketList (el varGrammar >>> el int) >>>
            ref)
  $ With (\global ->
            annotated "global" $
            position >>>
            swap >>>
            globalNameGrammar >>>
            global)
  $ With (\skolem ->
            annotated "skolem" $
            position >>>
            swap >>>
            list (
              el (sym ":skolem") >>>
              el (coerced int) >>>
              el varGrammar >>>
              el kindGrammar) >>>
            skolem)
  $ With (\meta ->
            annotated "meta" $
            position >>>
            swap >>>
            list (
              el (sym ":meta") >>>
              el (coerced int) >>>
              el kindGrammar) >>>
            meta)
  $ With (\unit ->
            annotated "unit" $
            position >>>
            swap >>>
            sym ":unit" >>>
            unit)
  $ With (\void ->
            annotated "void" $
            position >>>
            swap >>>
            sym ":void" >>>
            void)
  $ With (\arrow   ->
            annotated "arrow" $
            position >>>
            swap >>>
            sym "->" >>>
            arrow)
  $ With (\record  ->
            annotated "record" $
            position >>>
            swap >>>
            sym ":record" >>>
           record)
  $ With (\variant ->
            annotated "variant" $
            position >>>
            swap >>>
            sym ":variant" >>>
            variant)
  $ With (\present ->
            annotated "present" $
            position >>>
            swap >>>
            sym ":present" >>>
            present)
  $ With (\absent ->
            annotated "absent" $
            position >>>
            swap >>>
            sym ":absent" >>>
            absent)
  $ With (\extend ->
            annotated "extend" $
            position >>>
            swap >>>
            list (
              el (sym ":extend") >>>
              el (coerced symbol)) >>>
            extend)
  $ With (\nil' ->
            annotated "nil" $
            position >>>
            swap >>>
            sym ":nil" >>>
            nil')
  $ With (\app ->
            annotated "app" $
            position >>>
            swap >>>
            list (
              el typeGrammar >>>
              el typeGrammar) >>>
            app)
  $ With (\lambda ->
            annotated "lambda" $
            position >>>
            swap >>>
            list (
              el (sym ":lambda") >>>
              el varGrammar >>>
              el kindGrammar >>>
              el typeGrammar) >>>
           lambda)
  $ With (\forall ->
            annotated ":forall" $
            position >>>
            swap >>>
            list (
              el (sym ":forall") >>>
              el varGrammar >>>
              el kindGrammar >>>
              el typeGrammar) >>>
            forall)
  $ With (\mu ->
            annotated "mu" $
            position >>>
            swap >>>
            list (
              el (sym ":mu") >>>
              el varGrammar >>>
              el typeGrammar) >>>
            mu)
  $ End

definitionGrammar :: Grammar Position (Sexp :- t) (Definition :- t)
definitionGrammar =
  with $ \def -> list (
    el globalNameGrammar >>>
    el (list (rest paramGrammar)) >>>
    el typeGrammar) >>> def

programGrammar :: Grammar Position (Sexp :- t) (Program :- t)
programGrammar = fixGrammar $ match
  $ With (\letbinding ->
            annotated "let-binding" $
            position >>>
            swap >>>
            list (el (sym ":let") >>>
                  el definitionGrammar >>>
                  el programGrammar) >>>
            letbinding)
  $ With (\mutual ->
            annotated "letrec-binding" $
            position >>>
            swap >>>
            list (el (sym ":letrec") >>>
                  el (list (rest definitionGrammar)) >>>
                  el programGrammar) >>>
            mutual)
  $ With (\ret ->
            annotated "return-type" $
            position >>>
            swap >>>
            list (el (sym ":return") >>>
                  el typeGrammar) >>>
            ret)
  $ End
