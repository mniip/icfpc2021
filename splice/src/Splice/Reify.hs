module Splice.Reify
  ( reflectMod
  , reifyName
  , reifyTyThing
  ) where

import Data.List (foldl', find)

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import GHC
import GHC.ThToHs
import Unique
import OccName
import Name
import Var
import Id
import IdInfo
import ConLike
import DataCon
import PatSyn
import Module
import TyCoRep
import Type
import TcType
import TyCon
import PrelNames
import TysWiredIn
import BasicTypes
import FastString
import Outputable
import ErrUtils
import Util

import Splice.Mod

reflectRdrName :: TH.Name -> RdrName
reflectRdrName name = case thRdrNameGuesses name of
  rdrName:_ -> rdrName
  [] -> pprPanic "reflectRdrName" $ text $ show name

reflectExports :: Maybe [TH.Name] -> Maybe (Located [LIE GhcPs])
reflectExports = fmap $ noLoc . map (noLoc . IEVar noExtField . noLoc . IEName . noLoc . reflectRdrName)

reflectMod :: ModuleName -> Mod -> Either MsgDoc (HsModule GhcPs)
reflectMod modName (Mod exports decs) = do
  hsDecls <- convertToHsDecls Generated noSrcSpan decs
  pure HsModule
    { hsmodName = Just $ noLoc modName
    , hsmodExports = reflectExports exports
    , hsmodImports = []
    , hsmodDecls = hsDecls
    , hsmodDeprecMessage = Nothing
    , hsmodHaddockModHeader = Nothing
    }

-- Paraphrased from TcSplice:
reifyName :: NamedThing n => n -> TH.Name
reifyName thing
  | isExternalName name
              = mk_varg pkg_str mod_str occ_str
  | otherwise = TH.mkNameU occ_str (toInteger $ getKey (getUnique name))
  where
    name    = getName thing
    mod     = nameModule name
    pkg_str = unitIdString (moduleUnitId mod)
    mod_str = moduleNameString (moduleName mod)
    occ_str = occNameString occ
    occ     = nameOccName name
    mk_varg | isDataOcc occ = TH.mkNameG_d
            | isVarOcc  occ = TH.mkNameG_v
            | isTcOcc   occ = TH.mkNameG_tc
            | otherwise     = pprPanic "reifyName" (ppr name)

reifyTyThing :: TyThing -> Either MsgDoc TH.Info
-- The only reason this is monadic is for error reporting,
-- which in turn is mainly for the case when TH can't express
-- some random GHC extension
reifyTyThing (AnId id)
  = do  { ty <- reifyType (idType id)
        ; let v = reifyName id
        ; case idDetails id of
            ClassOpId cls -> return (TH.ClassOpI v ty (reifyName cls))
            RecSelId{sel_tycon=RecSelData tc}
                          -> return (TH.VarI (reifySelector id tc) ty Nothing)
            _             -> return (TH.VarI     v ty Nothing)
    }
-- reifyTyThing (ATyCon tc) = reifyTyCon tc
reifyTyThing (AConLike (RealDataCon dc))
  = do  { let name = dataConName dc
        ; ty <- reifyType (idType (dataConWrapId dc))
        ; return (TH.DataConI (reifyName name) ty
                              (reifyName (dataConOrigTyCon dc)))
        }
reifyTyThing (AConLike (PatSynCon ps))
  = do { let name = reifyName ps
       ; ty <- reifyPatSynType (patSynSig ps)
       ; return (TH.PatSynI name ty) }

reifyType :: Type -> Either MsgDoc TH.Type
-- Monadic only because of failure
reifyType ty                | tcIsLiftedTypeKind ty = return TH.StarT
  -- Make sure to use tcIsLiftedTypeKind here, since we don't want to confuse it
  -- with Constraint (#14869).
reifyType ty@(ForAllTy (Bndr _ argf) _)
                            = reify_for_all argf ty
reifyType (LitTy t)         = do { r <- reifyTyLit t; return (TH.LitT r) }
reifyType (TyVarTy tv)      = return (TH.VarT (reifyName tv))
reifyType (TyConApp tc tys) = reify_tc_app tc tys   -- Do not expand type synonyms here
reifyType ty@(AppTy {})     = do
  let (ty_head, ty_args) = splitAppTys ty
  ty_head' <- reifyType ty_head
  ty_args' <- reifyTypes (filter_out_invisible_args ty_head ty_args)
  pure $ mkThAppTs ty_head' ty_args'
  where
    -- Make sure to filter out any invisible arguments. For instance, if you
    -- reify the following:
    --
    --   newtype T (f :: forall a. a -> Type) = MkT (f Bool)
    --
    -- Then you should receive back `f Bool`, not `f Type Bool`, since the
    -- `Type` argument is invisible (#15792).
    filter_out_invisible_args :: Type -> [Type] -> [Type]
    filter_out_invisible_args ty_head ty_args =
      filterByList (map isVisibleArgFlag $ appTyArgFlags ty_head ty_args)
                   ty_args
reifyType ty@(FunTy { ft_af = af, ft_arg = t1, ft_res = t2 })
  | InvisArg <- af = reify_for_all Inferred ty  -- Types like ((?x::Int) => Char -> Char)
  | otherwise      = do { r1 <- reifyType t1; r2 <- reifyType t2; return (TH.ArrowT `TH.AppT` r1 `TH.AppT` r2) }
reifyType (CastTy t _)      = reifyType t -- Casts are ignored in TH
reifyType ty@(CoercionTy {})= noTH (sLit "coercions in types") (ppr ty)

reify_for_all :: ArgFlag -> Type -> Either MsgDoc TH.Type
-- Arg of reify_for_all is always ForAllTy or a predicate FunTy
reify_for_all argf ty = do
  tvs' <- reifyTyVars tvs
  case argToForallVisFlag argf of
    ForallVis   -> do phi' <- reifyType phi
                      pure $ TH.ForallVisT tvs' phi'
    ForallInvis -> do let (cxt, tau) = tcSplitPhiTy phi
                      cxt' <- reifyCxt cxt
                      tau' <- reifyType tau
                      pure $ TH.ForallT tvs' cxt' tau'
  where
    (tvs, phi) = tcSplitForAllTysSameVis argf ty

reifyTyLit :: TyLit -> Either MsgDoc TH.TyLit
reifyTyLit (NumTyLit n) = return (TH.NumTyLit n)
reifyTyLit (StrTyLit s) = return (TH.StrTyLit (unpackFS s))

reifyTypes :: [Type] -> Either MsgDoc [TH.Type]
reifyTypes = mapM reifyType

reifyPatSynType
  :: ([TyVar], ThetaType, [TyVar], ThetaType, [Type], Type) -> Either MsgDoc TH.Type
-- reifies a pattern synonym's type and returns its *complete* type
-- signature; see NOTE [Pattern synonym signatures and Template
-- Haskell]
reifyPatSynType (univTyVars, req, exTyVars, prov, argTys, resTy)
  = do { univTyVars' <- reifyTyVars univTyVars
       ; req'        <- reifyCxt req
       ; exTyVars'   <- reifyTyVars exTyVars
       ; prov'       <- reifyCxt prov
       ; tau'        <- reifyType (mkVisFunTys argTys resTy)
       ; return $ TH.ForallT univTyVars' req'
                $ TH.ForallT exTyVars' prov' tau' }

reifyKind :: Kind -> Either MsgDoc TH.Kind
reifyKind = reifyType

reifyCxt :: [PredType] -> Either MsgDoc [TH.Pred]
reifyCxt = mapM reifyType

reifyTyVars :: [TyVar] -> Either MsgDoc [TH.TyVarBndr]
reifyTyVars tvs = mapM reify_tv tvs
  where
    -- even if the kind is *, we need to include a kind annotation,
    -- in case a poly-kind would be inferred without the annotation.
    -- See #8953 or test th/T8953
    reify_tv tv = TH.KindedTV name <$> reifyKind kind
      where
        kind = tyVarKind tv
        name = reifyName tv

reify_tc_app :: TyCon -> [Type.Type] -> Either MsgDoc TH.Type
reify_tc_app tc tys
  = do { tys' <- reifyTypes (filterOutInvisibleTypes tc tys)
       ; maybe_sig_t (mkThAppTs r_tc tys') }
  where
    arity       = tyConArity tc

    r_tc | isUnboxedSumTyCon tc           = TH.UnboxedSumT (arity `div` 2)
         | isUnboxedTupleTyCon tc         = TH.UnboxedTupleT (arity `div` 2)
         | isPromotedTupleTyCon tc        = TH.PromotedTupleT (arity `div` 2)
             -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
         | isTupleTyCon tc                = if isPromotedDataCon tc
                                            then TH.PromotedTupleT arity
                                            else TH.TupleT arity
         | tc `hasKey` constraintKindTyConKey
                                          = TH.ConstraintT
         | tc `hasKey` funTyConKey        = TH.ArrowT
         | tc `hasKey` listTyConKey       = TH.ListT
         | tc `hasKey` nilDataConKey      = TH.PromotedNilT
         | tc `hasKey` consDataConKey     = TH.PromotedConsT
         | tc `hasKey` heqTyConKey        = TH.EqualityT
         | tc `hasKey` eqPrimTyConKey     = TH.EqualityT
         | tc `hasKey` eqReprPrimTyConKey = TH.ConT (reifyName coercibleTyCon)
         | isPromotedDataCon tc           = TH.PromotedT (reifyName tc)
         | otherwise                      = TH.ConT (reifyName tc)

    -- See Note [When does a tycon application need an explicit kind
    -- signature?] in TyCoRep
    maybe_sig_t th_type
      | tyConAppNeedsKindSig
          False -- We don't reify types using visible kind applications, so
                -- don't count specified binders as contributing towards
                -- injective positions in the kind of the tycon.
          tc (length tys)
      = do { let full_kind = tcTypeKind (mkTyConApp tc tys)
           ; th_full_kind <- reifyKind full_kind
           ; return (TH.SigT th_type th_full_kind) }
      | otherwise
      = return th_type

mkThAppTs :: TH.Type -> [TH.Type] -> TH.Type
mkThAppTs fun_ty arg_tys = foldl' TH.AppT fun_ty arg_tys

noTH :: PtrString -> SDoc -> Either MsgDoc a
noTH s d = Left $ hsep [text "Can't represent" <+> ptext s <+> text "in Template Haskell:", nest 2 d]

reifyFieldLabel :: FieldLabel -> TH.Name
reifyFieldLabel fl
  | flIsOverloaded fl
              = TH.Name (TH.mkOccName occ_str) (TH.NameQ (TH.mkModName mod_str))
  | otherwise = TH.mkNameG_v pkg_str mod_str occ_str
  where
    name    = flSelector fl
    mod     = nameModule name
    pkg_str = unitIdString (moduleUnitId mod)
    mod_str = moduleNameString (moduleName mod)
    occ_str = unpackFS (flLabel fl)

reifySelector :: Id -> TyCon -> TH.Name
reifySelector id tc
  = case find ((idName id ==) . flSelector) (tyConFieldLabels tc) of
      Just fl -> reifyFieldLabel fl
      Nothing -> pprPanic "reifySelector: missing field" (ppr id $$ ppr tc)
