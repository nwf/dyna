---------------------------------------------------------------------------
-- | Compile to Python
--
-- See bin/interpreter.py

-- Header material                                                      {{{
{-# LANGUAGE OverloadedStrings #-}

module Dyna.Backend.Python (pythonBackend) where

import           Control.Applicative ((<*))
import qualified Control.Arrow              as A
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString            as B
import qualified Data.ByteString.UTF8       as BU
import           Data.Char
-- import           Data.Either
import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
import qualified Data.Ord                   as O
import qualified Data.Set                   as S
import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
import           Dyna.Analysis.Base
import           Dyna.Analysis.Aggregation
import           Dyna.Analysis.RuleMode
import           Dyna.Main.BackendDefn
import           Dyna.Main.Exception
import           Dyna.Term.TTerm
import qualified Dyna.ParserHS.Parser       as P
import           Dyna.XXX.DataUtils (mapInOrApp)
import           Dyna.XXX.PPrint
import           Dyna.XXX.Trifecta (prettySpanLoc)
import           System.IO
import           Text.PrettyPrint.Free
import qualified Text.Trifecta              as T

------------------------------------------------------------------------}}}
-- DOpAMine Backend Information                                         {{{

-- At the moment, we pass through a @Maybe ()@ to indicate whether or not
-- we're making a call.  See the call to pycall in pdope_ below.
type PyDopeBS = ()

builtins :: BackendPossible PyDopeBS
builtins (f,is,o) = case () of
  _ | all isBound is && S.member (f,length is) constants
    -> case modeOf o of
         MFree  -> Right [OPIter o is f Det (Just ())]
         MBound -> let chkv = "_chk"
                   in Right $ [ OPIter (MF chkv) is f Det (Just ())
                              , OPCheq chkv (varOfMV o)
                              ]

  _ | f == "+"
    -> case (is,o) of
         ([x@(MB _),y@(MF _)],MB _) -> Right [OPIter y [o,x] "-" Det $ Just ()]
         ([x@(MF _),y@(MB _)],MB _) -> Right [OPIter x [o,y] "-" Det $ Just ()]
         _ -> Left True

  _ | f == "-"
    -> case (is,o) of
         ([x@(MB _),y@(MF _)],MB _) -> Right [OPIter y [x,o] "-" Det $ Just ()]
         ([x@(MF _),y@(MB _)],MB _) -> Right [OPIter x [o,y] "+" Det $ Just ()]
         _ -> Left True

  _ | S.member (f,length is) constants  -> Left True
  _ -> Left False

-- XXX This and pycall ought to be merged
constants :: S.Set (DFunct,Int)
constants = S.fromList
    [ ("+",2)
    , ("-",2)
    , ("-",1)    -- unary negation
    , ("*",2)
    , ("/",2)
    , ("^",2)
    , ("&",2)
    , ("|",2)
    , ("%",2)
    , ("**",2)
    , ("<",2)
    , ("<=",2)
    , (">",2)
    , (">=",2)
    , ("=",2)
    , ("!",1)
    , ("mod",1)
    , ("abs",1)
    , ("log",1)
    , ("exp",1)
    , ("and",2)
    , ("or",2)
    , ("not",1)
    , ("eval",1)
    , ("true",0)
    , ("false",0)
    , ("null",0)   -- XXX is this right?
    ]

------------------------------------------------------------------------}}}
-- DOpAMine Printout                                                    {{{

-- | Print functor and arity based on argument list
pfas f args = dquotes $ pretty f <> "/" <> (pretty $ length args)

pfa f n = parens $ dquotes $ pretty f <> "/" <> pretty n

-- pf f vs = pretty f <> (tupled $ map pretty vs)

functorIndirect table f vs = table <> (brackets $ pfas f vs)

-- this comes up because can't assign to ()
tupledOrUnderscore vs = if length vs > 0
                         then parens ((sepBy "," $ map pretty vs) <> ",")
                         else text "_"

filterBound = map (\(MF v) -> pretty v) . filter (not.isBound)

pslice vs = brackets $
       sepBy "," (map (\x -> case x of (MF _) -> ":" ; (MB v) -> pretty v) vs)
       <> "," -- add a list comma to ensure getitem is always passed a tuple.

pycall f vs = case (f, length vs) of
  (  "*", 2) -> infixOp " * "
  (  "+", 2) -> infixOp " + "
  (  "-", 2) -> infixOp " - "
  (  "/", 2) -> infixOp " / "
  (  "^", 2) -> infixOp " ** "
  (  "&", 2) -> infixOp " and " -- note: python's 'and' and 'or' operate on more than bool
  (  "|", 2) -> infixOp " or "
  (  "=", 2) -> infixOp " == "
  (  "<", 2) -> infixOp " < "
  ( "<=", 2) -> infixOp " <= "
  (  ">", 2) -> infixOp " > "
  ( ">=", 2) -> infixOp " >= "
  ( "==", 2) -> infixOp " == "
  ( "!=", 2) -> infixOp " != "
  ("mod", 2) -> infixOp " % "

  ("eval", 1) -> call "eval"   -- note: security hole.

  ( "abs", 1) -> call "abs"
  ( "log", 1) -> call "log"
  ( "exp", 1) -> call "exp"
  (   "!", 1) -> call "not"
  (   "-", 1) -> call "-"

  ( "null", 0) -> "None"
  ( "true", 0) -> "True"
  ("false", 0) -> "False"

  x            -> dynacPanic $ "Python.hs: Unknown request to pycall: "
                               <> pretty x

 where pretty_vs = map (pretty . varOfMV) vs
       call name = name <> (parens $ sepBy ", " $ pretty_vs)
       infixOp op = sepBy op $ pretty_vs


-- | Render a single dopamine opcode or its surrogate
pdope_ :: DOpAMine PyDopeBS -> Doc e
pdope_ (OPIndr _ _)   = dynacSorry "indirect evaluation not implemented"
pdope_ (OPAsgn v val) = pretty v <+> equals <+> pretty val
pdope_ (OPCheq v val) = "if" <+> pretty v <+> "!="
                             <+> pretty val <> ": continue"
pdope_ (OPCkne v val) = "if" <+> pretty v <+> "=="
                             <+> pretty val <> ": continue"
pdope_ (OPPeel vs i f) =
    "try:" `above` (indent 4 $
           tupledOrUnderscore vs
            <+> equals <> " "
                <> "peel" <> (parens $ pfas f vs <> comma <> pretty i)
     )
    -- you'll get a "TypeError: 'NoneType' is not iterable."
    `above` "except (TypeError, AssertionError): continue"
pdope_ (OPWrap v vs f) = pretty v
                           <+> equals
                           <+> "build"
                           <> (parens $ pfas f vs <> comma
                                <> (sepBy "," $ map pretty vs))

pdope_ (OPIter v vs f _ (Just ())) = pretty (varOfMV v)
                                     <+> equals
                                     <+> pycall f vs

pdope_ (OPIter o m f _ Nothing) =
      let mo = m ++ [o] in
          "for" <+> (tupledOrUnderscore $ filterBound mo)
                <+> "in" <+> functorIndirect "chart" f m <> pslice mo <> colon

-- | Render a dopamine sequence's checks and loops above a (indended) core.
pdope :: [DOpAMine PyDopeBS] -> Doc e -> Doc e
pdope _d _e =         (indent 4 $ "for _ in [None]:")
              `above` (indent 8 $ go _d _e)
 where
  go []  = id
  go (x:xs) = let indents = case x of OPIter _ _ _ d _ -> d /= Det ; _ -> False
              in above (pdope_ x)
                 . (if indents then indent 4 else id)
                 . go xs


printPlanHeader :: Handle -> Rule -> Cost -> IO ()
printPlanHeader h r c = do
  hPutStrLn h $ "# --"
    -- XXX This "prefixSD" thing is the only real reason we're doing this in
    -- IO; it'd be great if wl-pprint-extras understood how to prefix each
    -- line it was laying out.
  displayIO h $ prefixSD "# " $ renderPretty 1.0 100
                $ (prettySpanLoc $ r_span r) <> line
  hPutStrLn h $ "# Cost: " ++ (show c)

-- XXX This is unforunate and wrong, but our ANF is not quite right to
-- let us do this right.  See also Dyna.Analysis.RuleMode's use of this
-- function.
findHeadFA (Rule _ h _ _ _ (AS { as_assgn = as })) =
  case M.lookup h as of
    Nothing            -> error "No unification for head variable?"
    Just (Left _)      -> error "NTVar head?"
    Just (Right (f,a)) -> Just (f, length a)

printInitializer :: Handle -> Rule -> Action PyDopeBS -> IO ()
printInitializer fh rule@(Rule _ h _ r _ _) dope = do
  displayIO fh $ renderPretty 1.0 100
                 $ "@initializer" <> parens (uncurry pfa $ MA.fromJust $ findHeadFA rule)
                   `above` "def" <+> char '_' <> tupled [] <+> colon
                   `above` pdope dope emit
                   <> line
 where
   emit = "emit" <> tupled [pretty h, pretty r]

-- XXX INDIR EVAL
printUpdate :: Handle -> Rule -> Maybe DFunctAr -> (DVar, DVar) -> Action PyDopeBS -> IO ()
printUpdate fh rule@(Rule _ h _ r _ _) (Just (f,a)) (hv,v) dope = do
  displayIO fh $ renderPretty 1.0 100
                 $ "@register" <> parens (pfa f a)
                   `above` "def" <+> char '_' <> tupled (map pretty [hv,v]) <+> colon
                   `above` pdope dope emit
                   <> line
 where
   emit = "emit" <> tupled [pretty h, pretty r]

------------------------------------------------------------------------}}}
-- Driver                                                               {{{

driver :: BackendDriver PyDopeBS
driver am um qm is fh = do
  -- Aggregation mapping
  hPutStrLn fh $ "agg_decl = {}"
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
     forM_ ps $ \(r,c,vi,vo,act) -> do
       printPlanHeader fh r c
       printUpdate fh r fa (vi,vo) act

  hPutStrLn fh ""
  hPutStrLn fh $ "# ==Initializers=="
  forM_ is $ \(r,c,a) -> do
    printPlanHeader  fh r c
    printInitializer fh r a

  hPutStrLn fh $ "# ==Queries=="

  forM_ (M.toList qm) $ \(fa, ps) -> do
    hPutStrLn fh $ "# " ++ show fa
    forM_ ps $ \(r,c,qv,a) -> do
      printPlanHeader fh r c
      hPutStrLn fh $ "# " ++ show qv
      -- XXX
      -- displayIO fh $ renderPretty 1.0 100 $ pdope a "XXX"
      hPutStrLn fh ""


------------------------------------------------------------------------}}}
-- Export                                                               {{{

pythonBackend :: Backend
pythonBackend = Backend builtins constants driver

------------------------------------------------------------------------}}}
