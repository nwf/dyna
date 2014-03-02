---------------------------------------------------------------------------
-- | Compile to Python
--
-- See bin/interpreter.py

-- Header material                                                      {{{
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Dyna.Backend.Python.Backend (pythonBackend) where

-- import           Control.Applicative ((<*))
-- import qualified Control.Arrow              as A
import           Control.Exception (assert)
import           Control.Lens
                 ((^.), (%~), _1, makeLenses, use, view)
import           Control.Monad
import qualified Control.Monad.RWS          as RWS
import           Control.Monad.State
-- import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as B8
-- import qualified Data.ByteString.UTF8       as BU
-- import           Data.Char
-- import           Data.Either
import qualified Data.Foldable              as F
-- import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
-- import qualified Data.Ord                   as O
import qualified Data.Set                   as S
-- import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
import           Dyna.Analysis.ANFPretty
-- import           Dyna.Analysis.Aggregation
import           Dyna.Analysis.DOpAMine
import           Dyna.Analysis.Mode
import           Dyna.Analysis.QueryProcTable (QueryProcEntry(..))
import           Dyna.Analysis.RuleMode
import           Dyna.Analysis.UpdateProcTable (UpdateProcEntry(..),
                                                UpdateProcStatus(..))
import           Dyna.Backend.BackendDefn
import           Dyna.Backend.Primitives
import           Dyna.Main.Exception
import qualified Dyna.ParserHS.Types        as P
import qualified Dyna.ParserHS.Parser       as P
import           Dyna.Term.Normalized (NT (NTBase))
import           Dyna.Term.TTerm
import           Dyna.Term.SurfaceSyntax (dynaUnitTerm)
import           Dyna.XXX.PPrint
import           Dyna.XXX.MonadUtils
import           Dyna.XXX.Trifecta (prettySpanLoc)
import           System.IO
import           Text.PrettyPrint.Free
-- import qualified Text.Trifecta              as T

------------------------------------------------------------------------}}}
-- Supported aggregations                                               {{{

aggrs :: S.Set String
aggrs = S.fromList
  [ "max=" , "min="
  , "+=" , "*="
  , "&=" , "|="
  , ":-"
  , "="
  , "majority=" , "mean="
  , "set=" , "bag="
  , ":="
  , "dict="
  ]


------------------------------------------------------------------------}}}
-- DOpAMine Backend Information                                         {{{

-- | We can optionally attach a function to an 'OPIter' call,
-- which lets us permute arguments and so on when we go to do code
-- generation without having to re-probe the modes.
newtype PyDopeBS = PDBS (forall e . ModedVar -> [ModedVar] -> Doc e)

nfree, nuniv :: NIX DFunct
nfree = nHide (IFree)
nuniv = nHide (IUniv UShared)

-- | Note that isGround here is not quite the same as 'nGround': we do not
-- accept clobbered parameters.
isGround :: ModedVar -> Bool
isGround v = (v^.mv_mi) `nSub` nuniv

mpv :: [ModedVar] -> [Doc e]
mpv = map (pretty . (^.mv_var))

nnIn, nnOut :: (NIX DFunct, NIX DFunct)
nnIn  = (nuniv, nuniv)
nnOut = (nfree, nuniv)

mkFwdIter n d f p = QPE f nnOut (replicate n nnIn)
                        (\r as -> OPIter  (MV r nfree nuniv)
                                          (map (\a -> MV a nuniv nuniv) as)
                                          f d p)

addlConstQPEs = [ mkFwdIter 1 DetSemi "uniform" (call "uniform" []) -- XXX
                , mkFwdIter 2 DetSemi "uniform" (call "uniform" []) -- XXX
                , mkFwdIter 1 DetNon  "pycall"  (call "pycall"  [])
                , mkFwdIter 1 DetSemi "split"   (call "split"   [])
                , mkFwdIter 1 DetSemi "float"   (call "float"   [])
                , mkFwdIter 1 DetSemi "int"     (call "int"     [])
                , mkFwdIter 1 DetNon  "range"   (call "range"   [])
                , mkFwdIter 2 DetNon  "range"   (call "range"   [])
                , mkFwdIter 3 DetNon  "range"   (call "range"   [])
                , mkFwdIter 0 DetFail "null"    (zary "null"      ) -- XXX
                , mkFwdIter 0 Det     "nil"     (call "build" ["nil/0"])
                , mkFwdIter 2 Det     "cons"    (call "build" ["cons/2"])
                ]
 where
  zary :: (forall e . Doc e) -> Maybe PyDopeBS
  zary v     = Just $ PDBS $ \ _ _   -> v

  call :: (forall e . Doc e) -> (forall e . [Doc e]) -> Maybe PyDopeBS
  call fn as = Just $ PDBS $ \ _ vis ->
                fn <> (parens $ sepBy "," $ as ++ mpv vis)

addlNonConstQPEs = [ mkFwdIter 1 DetSemi "$key"    Nothing -- XXX primop?
                   ]

addlQPEs = addlConstQPEs ++ addlNonConstQPEs

addlUPEs = map (\(QPE f _ as _) -> UPE (f,length as) UPSForbidden) addlConstQPEs
        ++ map (\(QPE f _ as _) -> UPE (f,length as) UPSAccepted) addlNonConstQPEs



------------------------------------------------------------------------}}}
-- DOpAMine Printout                                                    {{{

renderPrimDat :: DPrimData -> Doc e
renderPrimDat x = case x of
  -- DPBool       x -> if x then "true" else "false"
  DPDollarNull   -> "null"
  DPDouble     d -> pretty d
  DPDQString   s -> pretty (show s) -- Yes, "pretty . show", so we get escaping
  DPInt        i -> pretty i
  DPNil          -> "build" <> parens (pfa ("nil" :: String) (0 :: Int))

ra, rai :: Monad m => Doc e -> m (Doc e -> Doc e)
ra      = return . above
rai x   = return (above x . indent 4)

renderPrimOp :: Monad m
             => Maybe (m (Doc e))
             -> Doc e
             -> DPrimOpF (Doc e)
             -> m (Doc e -> Doc e)
renderPrimOp mhv r x = case x of
  DPUnAbs   _   -> call   "abs"
  DPUnEnum  _   -> iter   (call_ "iter_cons")
  DPUnExp   _   -> call   "exp"
  DPUnLNeg  _   -> call   "not_"
  DPUnLog   _   -> call   "log"
  DPUnNNeg  _   -> call   "-"
  DPUnSqrt  _   -> call   "sqrt"

  DPBiAdd   _ _ -> incall "+"
  DPBiAnd   _ _ -> call   "and_"
  DPBiCmpEq _ _ -> call   "eq"
  DPBiCmpGe _ _ -> call   "gte"
  DPBiCmpGt _ _ -> call   "gt"
  DPBiCmpNe _ _ -> call   "not_eq"
  DPBiCmpLe _ _ -> call   "lte"
  DPBiCmpLt _ _ -> call   "lt"
  DPBiDiv   _ _ -> incall "/"
  DPBiExp   _ _ -> incall "**"
  DPBiIn    _ _ -> call   "in_list"
  DPBiIor   _ _ -> incall "or_"
  DPBiLog   _ _ -> call   "log"
  DPBiMod   _ _ -> incall "%"
  DPBiMul   _ _ -> incall "*"
  DPBiSub   _ _ -> incall "-"
  DPBiXor   _ _ -> incall "^"
 where
  xl = F.toList x
  call_  fn = fn <> (parens $ sepBy "," $ xl)
  call   fn = ra  (r <+> equals <+> call_ fn)
  incall fn = ra  (r <+> equals <+> (sepBy fn $ xl))
  iter   f = do
    pfx <- rai ("for" <+> r <+> "in" <+> f <> colon)
    (maybe return addTrack mhv) pfx
   where
    addTrack hv (core :: Doc e -> Doc e) = do
      tkv <- hv
      return (core . ((tkv <+> equals <+> r) `above`))

-- | Print functor and arity based on argument list
pfas :: Doc e -> [b] -> Doc e
pfas f args = dquotes $ f <> "/" <> (pretty $ length args)

pfa :: (Pretty f, Pretty n) => f -> n -> Doc e
pfa f n = dquotes $ pretty f <> "/" <> pretty n

-- pf f vs = pretty f <> (tupled $ map pretty vs)

functorIndirect :: Doc e -> Doc e -> [b] -> Doc e
functorIndirect table f vs = table <> (brackets $ pfas f vs)

-- this comes up because can't assign to ()
tupledOrUnderscore :: (Pretty a) => [a] -> Doc e
tupledOrUnderscore [] = text "_"
tupledOrUnderscore vs = parens ((sepBy "," $ map pretty vs) <> ",")


pslice :: [ModedVar] -> Doc e
pslice vs = brackets $
       sepBy "," (map (\x -> if isGround x then pretty (x^.mv_var) else ":") vs)
       <> "," -- add a comma to ensure getitem is always passed a tuple.

ground2underscore :: ModedVar -> Doc e
ground2underscore x = if isGround x then "_" else pretty (x^.mv_var)

piterate :: [ModedVar] -> Doc e
piterate vs = if length vs == 0 then "_"
              else "[" <> sepBy "," (map ground2underscore vs) <> "]"

-- filterGround :: [ModedVar] -> [DVar]
-- filterGround = map (^.mv_var) . filter (not.nGround.(^.mv_mi))

mkTkv :: RWS.RWS a () Int (Doc e)
mkTkv = do
  n <- incState id
  return $ "trk_" <> pretty n

newtype PDCtx = PDC { _pdc_temp  :: Int }
$(makeLenses ''PDCtx)

pdope_ :: S.Set DFunctAr -- XXX
       -> DOpAMine PyDopeBS
       -> RWS.RWS PDCtx () Int (Doc e -> Doc e)
pdope_ _  (OPAsnV l r  ) = ra (pretty l <+> equals <+> pretty r)
pdope_ _  (OPAsnP v p  ) = ra (pretty v <+> equals <+> renderPrimDat p)
pdope_ _  (OPCheq l r  ) = ra ("if not eq("
                            <> pretty l
                            <> comma
                            <> pretty r
                            <> "): continue")
pdope_ _  (OPCkne l r  ) = ra ("if eq("
                            <> pretty l
                            <> comma
                            <> pretty r
                            <> "): continue")

pdope_ _  (OPPeel vs i f _) = ra $
    "try:" `above` (indent 4 $
           tupledOrUnderscore vs
            <+> equals
                <+> "peel" <> (parens $ pfas (pretty f) vs <> comma <> pretty i)
    )
    -- you'll get a "TypeError: 'NoneType' is not iterable."
    `above` "except (TypeError, AssertionError): continue"
pdope_ _  (OPWrap v vs f) = ra $ pretty v
                                <+> equals
                                <+> "build"
                                <> (parens $ pfas (pretty f) vs <> comma
                                     <> (sepBy "," $ map pretty vs))

pdope_ _  (OPPrim v p _) = renderPrimOp (Just mkTkv) (pretty v) (fmap pretty p)

pdope_ _  (OPIter v vs _ Det (Just (PDBS c)))
  = ra $ pretty (v^.mv_var) <+> equals <+> c v vs

pdope_ _  (OPIter v vs _ DetNon (Just (PDBS c))) = do
      i <- incState id
      rai $ "for" <+> "d" <> pretty i
                  <> comma
                  <> piterate vs
                  <> comma
                  <> (ground2underscore v)
            <+> "in" <+> c v vs

pdope_ _ (OPIter v vs f d   (Just (PDBS c))) = dynacPanic $
           "Unexpected determinism flag (at python code gen):"
    <+> pretty v
    <+> pretty vs
    <+> squotes (pretty f)
    <+> text (show d)
    <+> parens (pretty $ c v vs)


pdope_ bc (OPIter o m f DetSemi Nothing) | (f,length m) `S.member` bc = do
  tkv <- mkTkv
  rai $
   assert (nFree $ o^.mv_mi) $
   assert (all (not . nFree . _mv_mi) m) $
   vcat
   [     pretty (o^.mv_var)
     <+> equals
     <+> "gbc(" <> pfas (pretty f) m
                <> ",[" <> sepBy "," (map (pretty . _mv_var) m) <> "])"
   , tkv
     <+> equals
     <+> ("build" <> tupled (pfas (pretty f) m : map (pretty . _mv_var) m))
   , "if" <+> pretty (o^.mv_var) <+> "is not None" <> colon
   ]

pdope_ bc (OPIter o m f _ Nothing) =
  assert (not $ (f,length m) `S.member` bc) $ do
      tkv <- incState id
      rai $ let mo = m ++ [o] in
          "for" <+> "trk_" <> pretty tkv <> "," <> piterate m <> comma <> (ground2underscore o)
                <+> "in" <+> functorIndirect "chart" (pretty f) m
                          <> pslice mo <> colon

    -- XXX Ought to make i and vs conditional on... doing debugging or the
    -- aggregator for this head caring.  The latter is a good bit more
    -- advanced than we are right now.
pdope_ _ (OPEmit h r i vs) = do
  ds <- use id

  -- A python map of variable name to value
  let varmap = brackets $ align $ fillPunct (comma <> space) $
         parens ("'nodes'" <> comma <> parens (hcat $ map (\x -> ("trk_" <> pretty x <> ",")) [0..ds-1]))
         : (map (\v -> let v' = pretty v in parens (dquotes v' <> comma <+> v')) vs)

  ra $ "emit" <> tupled [ pretty h
                        , pretty r
                        , pretty i
                        , "tuple" <> (parens $ varmap)
                        ]

pdope_ bc (OPBloc ds     ) = foldr (.) id `fmap` (mapM (pdope_ bc) ds)

pdope_ bc (OPScop f      ) = do
                           n <- view pdc_temp
                           let vn = B8.pack $ "tmp_" ++ (show n)
                           RWS.local (pdc_temp %~ (+1)) $ pdope_ bc (f vn)

pdope_ _  (OPIndr _ _    ) = dynacSorry "indirect evaluation not implemented"
pdope_ _  (OPAsgn v val  ) = ra (pretty v <+> equals <+> pretty val)

pdope bc d = indent 4 $ "for _ in [None]:"
             `above` indent 4 ((RWS.runRWS (pdope_ bc d) (PDC 0) 0 ^. _1) empty)

printPlanHeader :: Rule -> Cost -> Maybe Int -> Doc e
printPlanHeader r c mn = do
  vcat ["'''"
       , "Span:  " <+> (prettySpanLoc $ r_span r)
       , "RuleIx:" <+> (pretty $ r_index r)
       , "EvalIx:" <+> (pretty mn)
       , "Cost:  " <+> (pretty c)
       , "'''"]

printInitializer :: Handle -> S.Set DFunctAr
                 -> Rule -> Cost -> Actions PyDopeBS -> IO ()
printInitializer fh bc rule cost dope = do
  displayIO fh $ renderPretty 1.0 100
               $  "def" <+> char '_' <> tupled ["emit"] <> colon
                   `above` (indent 4 $ printPlanHeader rule cost Nothing)
                   `above` pdope bc (OPBloc dope)
                   <> line
                   <> "initializers.append"
                   <> parens (tupled [ pretty $ r_index rule
                                     , "_"
                                     , squotes $ prettySpanLoc $ r_span rule
                                     , "'''" <> (renderANF rule) <> "'''"
                                     ])
                   <> line
                   <> line
                   <> line

printUpdate :: Handle
            -> S.Set DFunctAr
            -> Rule -> Cost -> Int -> Maybe DFunctAr -> (DVar, DVar)
            -> Actions PyDopeBS -> IO ()
-- XXX INDIR EVAL
printUpdate _  _  _    _    _      Nothing      _      _    =
  dynacPanic "Python backend does not know how to do indirect evaluations"
printUpdate fh bc rule cost evalix (Just (f,a)) (hv,v) dope = do
  displayIO fh $ renderPretty 1.0 100
                 $ "#" <+> (parens $ pfa f a)
                   `above` "def" <+> char '_' <> tupled (map pretty [hv,v,"emit"]) <> colon
                   `above` (indent 4 $ printPlanHeader rule cost (Just evalix))
                   `above` pdope bc (OPBloc dope)
                   <> line
                   <> "updaters.append"
                     <> parens (tupled [parens $ pfa f a, pretty $ r_index rule, "_"])
                   <> line
                   <> line
                   <> line

printQuery :: Handle
           -> S.Set DFunctAr
           -> DFunctAr
           -> Rule
           -> [DVar]
           -> Cost
           -> Actions PyDopeBS
           -> IO ()
printQuery fh bc (f,a) rule vs cost dope = do
  displayIO fh $ renderPretty 1.0 100
                 $ "#" <+> (parens $ pfa f a)
                 `above` "def" <+> char '_'
                               <> tupled (map pretty vs ++ ["emit"])
                               <> colon
                 `above` (indent 4 $ printPlanHeader rule cost Nothing)
                 `above` pdope bc (OPBloc dope)
                 <> line
                 <> "queries.append"
                 <> parens (tupled [parens $ pfa f a
                                    , pretty $ r_index rule
                                    , "_"
                                    , squotes $ prettySpanLoc $ r_span rule
                                    , "'''" <> (renderANF rule) <> "'''"
                                    ])
                 <> line
                 <> line
                 <> line

------------------------------------------------------------------------}}}
-- Driver                                                               {{{

driver :: BackendDriver PyDopeBS
driver am ups is bc qp pr fh = do

  hPutStrLn fh "from __future__ import division"
  hPutStrLn fh "from stdlib import *"

  -- Parser resume state
  hPutStrLn fh "parser_state = \"\"\""
  hPutStrLn fh $ show pr
  -- XXX This is more than a little bit of a hack
  mapM_ (\((f,a),agg) -> hPutStrLn fh $ show
                                      $ P.renderPragma (P.PIAggr f a agg))
        $ M.toList am
  hPutStrLn fh "\"\"\""
  hPutStrLn fh ""
  hPutStrLn fh $ "queries = []"
  hPutStrLn fh $ "agg_decl = {}"
  hPutStrLn fh $ "rule = []"
  hPutStrLn fh $ "updaters = []"
  hPutStrLn fh $ "initializers = []"

  -- Aggregation mapping
  forM_ (M.toList am) $ \((f,a),v) -> do
     hPutStrLn fh $ show $    "agg_decl"
                           <> brackets (dquotes $ pretty f <> "/" <> pretty a)
                           <+> equals <+> (dquotes $ pretty v)

  hPutStrLn fh ""
  hPutStrLn fh $ "# ==Updates=="

  forM_ ups $ \(r,rps) -> do
     hPutStrLn fh ""
     hPutStrLn fh $ "# rix=" ++ (show $ r_index r)
     forM_ rps $ \(n,fa,c,vi,vo,act) -> do
       hPutStrLn fh $ "# " ++ show fa
       printUpdate fh bc r c n fa (vi,vo) act

  hPutStrLn fh ""
  hPutStrLn fh $ "# ==Initializers=="
  forM_ is $ \(r,c,a) -> do
    printInitializer fh bc r c a

  hPutStrLn fh $ "# ==Queries=="
  forM_ qp $ \(fa,r,(vs,(c,a))) -> printQuery fh bc fa r vs c a


------------------------------------------------------------------------}}}
-- Export                                                               {{{

pythonBackend :: Backend
pythonBackend = Backend (Just aggrs)
                        (\o is _ _ (PDBS e) -> e o is)
                        driver
                        addlQPEs
                        addlUPEs

------------------------------------------------------------------------}}}
