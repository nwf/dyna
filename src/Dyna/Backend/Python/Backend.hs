---------------------------------------------------------------------------
-- | Compile to Python
--
-- See bin/interpreter.py

-- Header material                                                      {{{
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Dyna.Backend.Python.Backend (pythonBackend) where

-- import           Control.Applicative ((<*))
-- import qualified Control.Arrow              as A
import           Control.Exception (assert)
import           Control.Lens ((^.))
import           Control.Monad
import           Control.Monad.State
-- import qualified Data.ByteString            as B
-- import qualified Data.ByteString.UTF8       as BU
-- import           Data.Char
-- import           Data.Either
-- import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
-- import qualified Data.Ord                   as O
import qualified Data.Set                   as S
-- import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
-- import           Dyna.Analysis.Aggregation
import           Dyna.Analysis.DOpAMine
import           Dyna.Analysis.Mode
import           Dyna.Analysis.RuleMode
import           Dyna.Backend.BackendDefn
import           Dyna.Main.Exception
import qualified Dyna.ParserHS.Types        as P
import qualified Dyna.ParserHS.Parser       as P
import           Dyna.Term.TTerm
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
  , "and=" , "or=" , "&=" , "|="
  , ":-"
  , "majority=" , "set=" , "bag="
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
nfree = nHide IFree
nuniv = nHide (IUniv UShared)

isGround, isFree :: ModedVar -> Bool
-- | Note that isGround here is not quite the same as 'nGround': we do not
-- accept clobbered parameters.
isGround v = (v^.mv_mi) `nSub` nuniv
isFree v   = (v^.mv_mi) `nSub` nfree

builtins :: BackendPossible PyDopeBS
builtins (f,is,o) = case () of
  _ | all isGround is
    -> maybe (Left False) gencall $ constants (f,length is)
        where
         gencall pc = case () of
                        _ | isFree o ->
                         Right $ BAct [OPIter o is f Det (Just pc)]
                                      [(o^.mv_var, nuniv)]
                        _ | isGround o ->
                          let chkv = "_chk"
                              fchk = MV chkv nfree nuniv
                          in Right $ BAct [ OPIter fchk is f Det (Just pc)
                                          , OPCheq chkv (o^.mv_var) ]
                                          []
                        _ -> Left True

  -- XXX These next two branches have nothing specific about Python at all
  -- and shouldn't be here.  Similarly, the corresponding entries in
  -- NoBackend also shouldn't be around (possibly).  These should be handled
  -- much more generically earlier in the pipeline.
  _ | f == "+" && isGround o
    -> case is of
         [x,y] | isGround x && isFree y
               -> Right $ BAct [OPIter y [o,x] "-" Det (Just$ PDBS$ infixOp "-")]
                               [(y^.mv_var, nuniv)]
         [x,y] | isFree x && isGround y
               -> Right $ BAct [OPIter x [o,y] "-" Det (Just$ PDBS$ infixOp "-")]
                               [(x^.mv_var, nuniv)]
         _ -> Left True

  _ | f == "-" && isGround o
    -> case is of
         [x,y] | isGround x && isFree y
               -> Right $ BAct [OPIter y [x,o] "-" Det (Just$ PDBS$ infixOp "-")]
                               [(y^.mv_var, nuniv)]
         [x,y] | isFree x && isGround y
               -> Right $ BAct [OPIter x [o,y] "+" Det (Just$ PDBS$ infixOp "+")]
                               [(x^.mv_var, nuniv)]
         _ -> Left True

  _ | MA.isJust (constants (f,length is)) -> Left True
  _ -> Left False

infixOp :: Doc e -> a -> [ModedVar] -> Doc e
infixOp op _ vis = sepBy op $ mpv vis

mpv :: [ModedVar] -> [Doc e]
mpv = map (pretty . (^.mv_var))

constants :: DFunctAr -> Maybe PyDopeBS
constants = go
 where
  go ("-",1)     = Just $ PDBS $ call "-" []
  go ("^",2)     = Just $ PDBS $ infixOp "^"
  go ("|",2)     = Just $ PDBS $ infixOp "|"
  go ("-",2)     = Just $ PDBS $ infixOp "-"
  go ("/",2)     = Just $ PDBS $ infixOp "/"
  go ("*",2)     = Just $ PDBS $ infixOp "*"
  go ("**",2)    = Just $ PDBS $ infixOp "**"
  go ("&",2)     = Just $ PDBS $ infixOp "&"
  go ("%",2)     = Just $ PDBS $ infixOp "%"
  go ("+",2)     = Just $ PDBS $ infixOp "+"

  go ("mod",2)   = Just $ PDBS $ infixOp "%"
  go ("abs",1)   = Just $ PDBS $ call "abs" []
  go ("log",1)   = Just $ PDBS $ call "log" []
  go ("exp",1)   = Just $ PDBS $ call "exp" []



  go ("pycall", _) = Just $ PDBS $ call "pycall" []
  go ("getattr", 2) = Just $ PDBS $ call "getattr" []


  go ("uniform", _) = Just $ PDBS $ call "uniform" []

  go ("<=",2)    = Just $ PDBS $ infixOp "<="
  go ("<",2)     = Just $ PDBS $ infixOp "<"
  go ("=",2)     = Just $ PDBS $ infixOp "=="
  go ("==",2)    = Just $ PDBS $ infixOp "=="
  go (">=",2)    = Just $ PDBS $ infixOp ">="
  go (">",2)     = Just $ PDBS $ infixOp ">"
  go ("!=",2)    = Just $ PDBS $ infixOp "!="

  go ("and",2)   = Just $ PDBS $ infixOp "and"
  go ("or",2)    = Just $ PDBS $ infixOp "or"

  go ("true",0)  = Just $ PDBS $ nullary "True"
  go ("false",0) = Just $ PDBS $ nullary "False"
  go ("null",0)  = Just $ PDBS $ nullary "None"

  go ("!",1)     = Just $ PDBS $ call "not" []
  go ("not",1)   = Just $ PDBS $ call "not" []

  go ("eval",1)  = Just $ PDBS $ call "None;exec " []
  go ("tuple",_) = Just $ PDBS $ call "" []

  go ("nil",0)   = Just $ PDBS $ call "build" ["nil/0"]
  go ("cons",2)  = Just $ PDBS $ call "build" ["cons/2"]

  go _           = Nothing

  nullary v     _ _   = v
  call    fn as _ vis = fn <> (parens $ sepBy "," $ as ++ mpv vis)

------------------------------------------------------------------------}}}
-- DOpAMine Printout                                                    {{{

-- | Print functor and arity based on argument list
pfas :: Pretty a => a -> [b] -> Doc e
pfas f args = dquotes $ pretty f <> "/" <> (pretty $ length args)

pfa :: (Pretty f, Pretty n) => f -> n -> Doc e
pfa f n = parens $ dquotes $ pretty f <> "/" <> pretty n

-- pf f vs = pretty f <> (tupled $ map pretty vs)

functorIndirect :: Pretty a => Doc e -> a -> [b] -> Doc e
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
              else parens $
       sepBy "," (map ground2underscore vs)
       <> "," -- add a comma to ensure tuple.


-- filterGround :: [ModedVar] -> [DVar]
-- filterGround = map (^.mv_var) . filter (not.nGround.(^.mv_mi))

-- | Render a single dopamine opcode or its surrogate
pdope_ :: S.Set DFunctAr -> DOpAMine PyDopeBS -> State Int (Doc e)
pdope_ _ (OPIndr _ _)   = dynacSorry "indirect evaluation not implemented"
pdope_ _ (OPAsgn v val) = return $ pretty v <+> equals <+> pretty val
pdope_ _ (OPCheq v val) = return $ "if" <+> pretty v <+> "!="
                                      <+> pretty val <> ": continue"
pdope_ _ (OPCkne v val) = return $ "if" <+> pretty v <+> "=="
                                      <+> pretty val <> ": continue"
pdope_ _ (OPPeel vs i f _) = return $
    "try:" `above` (indent 4 $
           tupledOrUnderscore vs
            <+> equals
                <+> "peel" <> (parens $ pfas f vs <> comma <> pretty i)
    )
    -- you'll get a "TypeError: 'NoneType' is not iterable."
    `above` "except (TypeError, AssertionError): continue"
pdope_ _ (OPWrap v vs f) = return $ pretty v
                           <+> equals
                           <+> "build"
                           <> (parens $ pfas f vs <> comma
                                <> (sepBy "," $ map pretty vs))

pdope_ _ (OPIter v vs _ Det (Just (PDBS c))) = return $ pretty (v^.mv_var)
                                     <+> equals
                                     <+> c v vs

pdope_ _ (OPIter v vs f d   (Just (PDBS c))) = dynacPanic $
           "Unexpected determinism flag (at python code gen):"
    <+> pretty v
    <+> pretty vs
    <+> squotes (pretty f)
    <+> text (show d)
    <+> parens (pretty $ c v vs)

-- XXX This works only for the special case at hand (thus the asserts)
pdope_ bc (OPIter o m f DetSemi Nothing) | (f,length m) `S.member` bc = do
  dookie <- incState
  return $
   assert (iIsFree $ nExpose $ o^.mv_mi) $
   assert (all (not . iIsFree . nExpose . _mv_mi) m) $
   vcat
   [     pretty (o^.mv_var)
     <+> equals
     <+> "gbc"
     <> tupled (pfas f m : map (pretty . _mv_var) m)

--- needs an opbuild
   , ("d" <> pretty dookie)
     <+> equals
     <+> ("build" <> tupled (pfas f m : map (pretty . _mv_var) m))

   , "if" <+> pretty (o^.mv_var) <+> "is not None" <> colon

   ]

pdope_ bc (OPIter o m f _ Nothing) =
  assert (not $ (f,length m) `S.member` bc) $ do
      i <- incState
      return $ let mo = m ++ [o] in
          "for" <+> "d" <> pretty i <> "," <> piterate m <> comma <> (ground2underscore o)
                <+> "in" <+> functorIndirect "chart" f m <> pslice mo <> colon

    -- XXX Ought to make i and vs conditional on... doing debugging or the
    -- aggregator for this head caring.  The latter is a good bit more
    -- advanced than we are right now.
pdope_ _ (OPEmit h r i vs) = do
  ds <- get

  -- A python map of variable name to value
  let varmap = brackets $ align $ fillPunct (comma <> space) $
         parens ("'nodes'" <> comma <> parens (hcat $ map (\x -> ("d" <> pretty x <> ",")) [0..ds-1]))
         : (map (\v -> let v' = pretty v in parens (dquotes v' <> comma <+> v')) vs)

  return $ "emit" <> tupled [ pretty h
                            , pretty r
                            , pretty i
                            , "tuple" <> (parens $ varmap)
                            ]

-- | Render a dopamine sequence's checks and loops above a (indended) core.
pdope :: S.Set DFunctAr -> Actions PyDopeBS -> Doc e
pdope bc _d =         (indent 4 $ "for _ in [None]:")
              `above` (indent 8 $ evalState (go _d) 0)
 where
  go []  = return empty
  go (x:xs) = let indents = case x of OPIter _ _ _ d _ -> d /= Det ; _ -> False
              in do
                   x' <- pdope_ bc x
                   xs' <- go xs
                   return $ x' `above` ((if indents then indent 4 else id) xs')


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
                 $ "def" <+> char '_' <> tupled ["emit"] <> colon
                   `above` (indent 4 $ printPlanHeader rule cost Nothing)
                   `above` pdope bc dope
                   <> line
                   <> "initializers.append((" <> (pretty $ r_index rule) <> ", _" <> "))"
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
                 $ "#" <+> (pfa f a)
                   `above` "def" <+> char '_' <> tupled (map pretty [hv,v,"emit"]) <> colon
                   `above` (indent 4 $ printPlanHeader rule cost (Just evalix))
                   `above` pdope bc dope
                   <> line
                   <> "updaters.append"
                     <> parens (tupled [pfa f a, pretty $ r_index rule, "_"])
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
                 $ "#" <+> (pfa f a)
                 `above` "def" <+> char '_'
                               <> tupled (map pretty vs ++ ["emit"])
                               <> colon
                 `above` (indent 4 $ printPlanHeader rule cost Nothing)
                 `above` pdope bc dope
                 <> line
                 <> "queries.append"
                 <> parens (tupled [pfa f a, pretty $ r_index rule, "_"])
                 <> line
                 <> line
                 <> line


------------------------------------------------------------------------}}}
-- Driver                                                               {{{

driver :: BackendDriver PyDopeBS
driver am um is bc qp pr fh = do

  hPutStrLn fh "from __future__ import division"

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
  hPutStrLn fh $ "updaters = []"
  hPutStrLn fh $ "initializers = []"

  -- Aggregation mapping
  forM_ (M.toList am) $ \((f,a),v) -> do
     hPutStrLn fh $ show $    "agg_decl"
                           <> brackets (dquotes $ pretty f <> "/" <> pretty a)
                           <+> equals <+> (dquotes $ pretty v)

  hPutStrLn fh ""
  hPutStrLn fh $ "# ==Updates=="

  -- plans aggregated by functor/arity
  forM_ (M.toList um) $ \(fa, ps) -> do
     hPutStrLn fh ""
     hPutStrLn fh $ "# " ++ show fa
     forM_ ps $ \(r,n,c,vi,vo,act) -> do
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
                        builtins
                        (MA.isJust . constants)
                        (\o is _ _ (PDBS e) -> e o is)
                        driver

------------------------------------------------------------------------}}}
