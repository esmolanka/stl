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
import Data.Text.Prettyprint.Doc as PP
  ( Pretty(..), Doc,  (<+>), vsep, indent, colon, squotes
  , parens, hsep, group, braces, angles, brackets, line
  )
import GHC.Generics
import Language.Sexp.Located (Position(..), dummyPos)

data Kind
  = Star
  | Row
  | Presence
  | Arr Kind Kind
  deriving (Show, Eq, Ord, Generic)

instance Pretty Kind where
  pretty = ppKind False
    where
      parensIf :: Bool -> Doc a -> Doc a
      parensIf True = parens
      parensIf False = id

      ppKind :: Bool -> Kind -> Doc a
      ppKind nested = \case
        Star -> "Star"
        Row -> "Row"
        Presence -> "Presence"
        Arr f a -> parensIf nested $
          ppKind True f <+> "->" <+> ppKind False a

newtype Var = Var Text
  deriving (Show, Eq, Ord, IsString)

instance Pretty Var where
  pretty (Var name) = pretty name

newtype Label = Label Text
  deriving (Show, Eq, Ord, IsString)

instance Pretty Label where
  pretty (Label name) = squotes (pretty name)

newtype MetaVar = MetaVar Int
  deriving (Eq, Ord, Show)

newtype Skolem = Skolem Int
  deriving (Eq, Ord, Show)

newtype GlobalName = GlobalName Text
  deriving (Eq, Ord, Show, IsString)

instance Pretty GlobalName where
  pretty (GlobalName name) =
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

instance Pretty (TypeF (Fix TypeF)) where
  pretty = Fix >>> ppType 0
    where
      parensIf :: Bool -> Doc a -> Doc a
      parensIf True = parens
      parensIf False = id

      ppTele :: TypeF Type -> [Type] -> Doc a
      ppTele f args = hsep (ppTypeCon 1 f : map (ppType 1) args)

      ppTypeCon :: Int -> TypeF Type -> Doc a
      ppTypeCon lvl = \case
        TRef _ x n -> if n > 0 then pretty x <> "/" <> pretty n else pretty x
        TGlobal _ name -> pretty name
        TSkolem _ (Skolem name) hint _ -> pretty hint <> brackets (pretty name)
        TMeta _ (MetaVar name) k ->
          if k == Star
          then "?" <> pretty name
          else parens ("?" <> pretty name <+> colon <+> pretty k)
        TUnit _ -> "Unit"
        TVoid _ -> "Void"
        TArrow _ -> "(->)"
        TRecord _ -> "Record"
        TVariant _ -> "Variant"
        TPresent _ -> "▪︎"
        TAbsent _ -> "▫︎"
        TExtend _ lbl -> "Extend" <+> pretty lbl
        TNil _ -> "Nil"
        TApp _ f a -> parensIf (lvl > 1) $ ppType 1 f <+> ppType 2 a
        TLambda _ x k b ->
          let variable = if k == Star then pretty x else parens (pretty x <+> colon <+> pretty k)
          in parensIf (lvl > 0) $ "λ" <+> variable <> "." <+> ppType 1 b
        TForall _ x k b ->
          let variable = if k == Star then pretty x else parens (pretty x <+> colon <+> pretty k)
          in parensIf (lvl > 0) $ "∀" <+> variable <> "." <+> ppType 1 b
        TMu _ x b -> parensIf (lvl > 0) $ "μ" <+> pretty x <> "." <+> ppType 1 b

      ppType :: Int -> Type -> Doc a
      ppType lvl = tele >>> \case
        (Fix (TArrow _), [a, b]) -> parensIf (lvl > 0) $ ppType 1 a <+> "->" <+> ppType 0 b
        (Fix (TRecord _), [Fix (TNil _)]) -> braces mempty
        (Fix (TVariant _), [Fix (TNil _)]) -> angles mempty
        (Fix (TRecord _), [row]) -> group $ braces $ ppType 0 row
        (Fix (TVariant _), [row]) -> group $ angles $ ppType 0 row
        (Fix (TExtend _ (Label lbl)), [presence, ty, row]) ->
          let fieldName =
                case presence of
                  Fix (TPresent _) -> pretty lbl
                  Fix (TAbsent _)  -> "¬" <> pretty lbl
                  _other           -> pretty lbl <> "^" <> ppType 2 presence
              rest =
                case tele row of
                  (Fix (TNil _), _) -> mempty
                  (Fix (TExtend _ _), _) -> "," <+> ppType 0 row
                  _other -> " |" <+> ppType 0 row
          in fieldName <+> ":" <+> ppType 0 ty <> rest
        (Fix otherTyCon, [])     -> ppTypeCon lvl otherTyCon
        (Fix otherTyCon, rest)   -> parensIf (lvl > 0) $ ppTele otherTyCon rest

instance Pretty (Fix TypeF) where
  pretty (Fix a) = pretty a

instance {-# OVERLAPPING #-} Show (Fix TypeF) where
  showsPrec n x = showParen (n > 1) $ shows (pretty x)

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

instance Pretty Definition where
  pretty (Definition name params ty) =
    hsep $ [ "type", pretty name ]
        ++ map ppParam params
        ++ [ "=", pretty ty ]
    where
      ppParam (var, kind) =
        if kind == Star then pretty var else parens (pretty var <+> colon <+> pretty kind)

data ProgramF e
  = PLet    Position Definition e
  | PMutual Position [Definition] e
  | PReturn Position Type
  deriving (Functor, Foldable, Traversable, Generic)

type Program = Fix ProgramF

instance Pretty (ProgramF Program) where
  pretty = \case
    PLet _ def rest ->
      vsep [ pretty def <> line, pretty rest ]
    PMutual _ defs rest ->
      vsep [ "mutual" <+> "{"
           , indent 4 $ vsep $ map pretty defs
           , "}" <> line
           , pretty rest
           ]
    PReturn _ ty ->
      pretty ty

instance Pretty (Fix ProgramF) where
  pretty (Fix a) = pretty a
