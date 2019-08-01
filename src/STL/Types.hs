{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module STL.Types
  ( module STL.Types
  , Position(..)
  , dummyPos
  ) where

import Control.Category ((>>>))
import Control.Monad.Reader

import Data.Char (isUpper)
import Data.Functor.Foldable (Fix(..), para)
import Data.List (foldl')
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import STL.Pretty
import STL.Syntax.Position

data Kind
  = Star
  | Row
  | Presence
  | Arr Kind Kind
  deriving (Show, Eq, Ord, Generic)

instance CPretty Kind where
  cpretty = aKind . ppKind' False
    where
      parensIf :: Bool -> Doc a -> Doc a
      parensIf True = parens
      parensIf False = id

      ppKind' :: Bool -> Kind -> Doc a
      ppKind' nested = \case
        Star -> "Star"
        Row -> "Row"
        Presence -> "Prs"
        Arr f a -> parensIf nested $
          ppKind' True f <+> "->" <+> ppKind' False a

newtype Var = Var Text
  deriving (Show, Eq, Ord, IsString)

instance CPretty Var where
  cpretty (Var name) = aVariable (pretty name)

newtype Label = Label Text
  deriving (Show, Eq, Ord, IsString)

instance CPretty Label where
  cpretty (Label name) = aLabel (pretty name)

newtype MetaVar = MetaVar Int
  deriving (Eq, Ord, Show)

newtype Skolem = Skolem Int
  deriving (Eq, Ord, Show)

newtype GlobalName = GlobalName Text
  deriving (Eq, Ord, Show, IsString)

instance CPretty GlobalName where
  cpretty (GlobalName name) =
    aConstructor $
      case T.uncons name of
        Nothing -> "$"
        Just (n, _) -> if isUpper n then pretty name else "$" <> pretty name

data TypeF e
  = TRef      { _getPosition :: Position, _refName :: Var, _refIndex :: Int }
  | TGlobal   { _getPosition :: Position, _globalName :: GlobalName }
  | TSkolem   { _getPosition :: Position, _skolemName :: Skolem, _skolemHint :: Var, _skolemKind :: Kind }
  | TMeta     { _getPosition :: Position, _metaName :: MetaVar, _metaKind :: Kind }
  | TUnit     { _getPosition :: Position }
  | TVoid     { _getPosition :: Position }
  | TArrow    { _getPosition :: Position }
  | TRecord   { _getPosition :: Position }
  | TVariant  { _getPosition :: Position }
  | TPresent  { _getPosition :: Position }
  | TAbsent   { _getPosition :: Position }
  | TExtend   { _getPosition :: Position, _extLabel :: Label }
  | TNil      { _getPosition :: Position }
  | TApp      { _getPosition :: Position, _appFun :: e, _appArg :: e }
  | TLambda   { _getPosition :: Position, _lambdaName :: Var, _lambdaKind :: Kind, _lambdaBody :: e }
  | TForall   { _getPosition :: Position, _forallName :: Var, _forallKind :: Kind, _forallBody :: e }
  | TMu       { _getPosition :: Position, _muName :: Var, _muBody :: e }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

type Type = Fix TypeF

instance CPretty (TypeF (Fix TypeF)) where
  cpretty = ppType . Fix

instance CPretty (Fix TypeF) where
  cpretty = ppType

ppType :: Type -> Doc AnsiStyle
ppType = ppType' 0
  where
    parensIf :: Bool -> Doc a -> Doc a
    parensIf True = parens
    parensIf False = id

    ppTele :: TypeF Type -> [Type] -> Doc AnsiStyle
    ppTele f args = hsep (ppTypeCon 1 f : map (ppType' 1) args)

    ppTypeCon :: Int -> TypeF Type -> Doc AnsiStyle
    ppTypeCon lvl = \case
      TRef _ x n -> aVariable (if n > 0 then cpretty x <> "/" <> pretty n else cpretty x)
      TGlobal _ name -> cpretty name
      TSkolem _ (Skolem name) hint _ -> cpretty hint <> brackets (pretty name)
      TMeta _ (MetaVar name) k ->
        if k == Star
        then "?" <> pretty name
        else parens ("?" <> pretty name <+> colon <+> cpretty k)
      TUnit _ -> aConstructor "Unit"
      TVoid _ -> aConstructor "Void"
      TArrow _ -> aConstructor "(->)"
      TRecord _ -> aConstructor "Record"
      TVariant _ -> aConstructor "Variant"
      TPresent _ -> aConstructor "▪︎"
      TAbsent _ -> aConstructor "▫︎"
      TExtend _ lbl -> aConstructor "Extend" <+> cpretty lbl
      TNil _ -> aConstructor "Nil"
      TApp _ f a -> parensIf (lvl > 1) $ ppType' 1 f <+> ppType' 2 a
      TLambda _ x k b ->
        let var = if k == Star then cpretty x else parens (cpretty x <+> colon <+> cpretty k)
        in parensIf (lvl > 0) $ aKeyword "λ" <+> var <> "." <+> ppType' 1 b
      TForall _ x k b ->
        let var = if k == Star then cpretty x else parens (cpretty x <+> colon <+> cpretty k)
        in parensIf (lvl > 0) $ aKeyword "∀" <+> var <> "." <+> ppType' 1 b
      TMu _ x b -> parensIf (lvl > 0) $ aKeyword "μ" <+> cpretty x <> "." <+> ppType' 1 b

    ppType' :: Int -> Type -> Doc AnsiStyle
    ppType' lvl = tele >>> \case
      (Fix (TArrow _), [a, b]) -> parensIf (lvl > 0) $ ppType' 1 a <+> "->" <+> ppType' 0 b
      (Fix (TRecord _), [Fix (TNil _)]) -> braces mempty
      (Fix (TVariant _), [Fix (TNil _)]) -> angles mempty
      (Fix (TRecord _), [row]) -> group $ braces $ ppType' 0 row
      (Fix (TVariant _), [row]) -> group $ angles $ ppType' 0 row
      (Fix (TExtend _ (Label lbl)), [presence, ty, row]) ->
        let fieldName =
              case presence of
                Fix (TPresent _) -> aLabel (pretty lbl)
                Fix (TAbsent _)  -> "¬" <> aLabel (pretty lbl)
                _other           -> aLabel (pretty lbl) <> "^" <> ppType' 2 presence
            rest =
              case tele row of
                (Fix (TNil _), _) -> mempty
                (Fix (TExtend _ _), _) -> "," <+> ppType' 0 row
                _other -> " |" <+> ppType' 0 row
        in fieldName <+> ":" <+> ppType' 0 ty <> rest
      (Fix otherTyCon, [])     -> ppTypeCon lvl otherTyCon
      (Fix otherTyCon, rest)   -> parensIf (lvl > 0) $ ppTele otherTyCon rest


instance {-# OVERLAPPING #-} Show (Fix TypeF) where
  showsPrec n x = showParen (n > 1) $ shows (ppType x)

----------------------------------------------------------------------

getPosition :: Type -> Position
getPosition (Fix t) = _getPosition t

tele :: Type -> (Type, [Type])
tele expr = runReader (para alg expr) []
  where
    alg :: TypeF (Type, Reader [Type] (Type, [Type])) -> Reader [Type] (Type, [Type])
    alg = \case
      TApp _ (_, f) (r, _) ->
        local (r :) f
      other -> do
        collected <- ask
        return (Fix (fmap fst other), collected)

untele :: Type -> [Type] -> Type
untele f args =
  foldl' ((Fix .) . TApp (getPosition f)) f args

----------------------------------------------------------------------

data Definition = Definition
  { defName   :: GlobalName
  , defParams :: [(Var, Kind)]
  , defType   :: Type
  } deriving (Generic)

instance CPretty Definition where
  cpretty (Definition name params ty) =
    hsep $ [ aKeyword "type", cpretty name ]
        ++ map ppParam params
        ++ [ "=", cpretty ty ]
    where
      ppParam (var, kind) =
        if kind == Star
        then cpretty var
        else parens (cpretty var <+> colon <+> cpretty kind)

data ProgramF e
  = PLet    Position Definition e
  | PMutual Position [Definition] e
  | PReturn Position Type
  deriving (Functor, Foldable, Traversable, Generic)

type Program = Fix ProgramF

instance CPretty (ProgramF Program) where
  cpretty = \case
    PLet _ def rest ->
      vsep [ cpretty def <> line, cpretty rest ]
    PMutual _ defs rest ->
      vsep [ aKeyword "mutual" <+> "{"
           , indent 4 $ vsep $ map cpretty defs
           , "}" <> line
           , cpretty rest
           ]
    PReturn _ ty ->
      cpretty ty

instance CPretty (Fix ProgramF) where
  cpretty (Fix a) = cpretty a
