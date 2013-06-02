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
-- import           Control.Exception
import           Control.Lens ((^.))
import           Control.Monad
-- import qualified Data.ByteString            as B
-- import qualified Data.ByteString.UTF8       as BU
-- import           Data.Char
-- import           Data.Either
-- import qualified Data.List                  as L
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
-- import qualified Data.Ord                   as O
-- import qualified Data.Set                   as S
-- import qualified Debug.Trace                as XT
import           Dyna.Analysis.ANF
-- import           Dyna.Analysis.Aggregation
import           Dyna.Analysis.DOpAMine
import           Dyna.Analysis.Mode
import           Dyna.Analysis.RuleMode
import           Dyna.Backend.BackendDefn
import           Dyna.Main.Exception
import           Dyna.Term.TTerm
-- import qualified Dyna.ParserHS.Parser       as P
import           Dyna.XXX.PPrint
import           Dyna.XXX.Trifecta (prettySpanLoc)
import           System.IO
import           Text.PrettyPrint.Free
-- import qualified Text.Trifecta              as T

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
isGround v = nGround (v^.mv_mi)
isFree v = nSub (v^.mv_mi) nfree

builtins :: BackendPossible PyDopeBS
builtins (f,is,o) = case () of
  _ | all isGround is 
    -> maybe (Left False) gencall $ M.lookup (f,length is) constants
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

  _ | M.member (f,length is) constants  -> Left True
  _ -> Left False

infixOp op _ vis = sepBy op $ mpv vis
mpv = map (pretty . (^.mv_var))

constants :: M.Map (DFunct,Int) PyDopeBS
constants = M.fromList
    [(("+",2)     , PDBS $ infixOp "+"    )
    ,(("-",2)     , PDBS $ infixOp "-"    )
    ,(("*",2)     , PDBS $ infixOp "*"    )
    ,(("/",2)     , PDBS $ infixOp "/"    )
    ,(("^",2)     , PDBS $ infixOp "^"    )
    ,(("&",2)     , PDBS $ infixOp "&"    )
    ,(("|",2)     , PDBS $ infixOp "|"    )
    ,(("%",2)     , PDBS $ infixOp "%"    )
    ,(("**",2)    , PDBS $ infixOp "**"   )
    ,(("==",2)    , PDBS $ infixOp "=="   )
    ,(("!=",2)    , PDBS $ infixOp "!="   )
    ,(("<",2)     , PDBS $ infixOp "<"    )
    ,(("<=",2)    , PDBS $ infixOp "<="   )
    ,((">",2)     , PDBS $ infixOp ">"    )
    ,((">=",2)    , PDBS $ infixOp ">="   )
    ,(("=",2)     , PDBS $ infixOp "="    )
    ,(("!=",2)    , PDBS $ infixOp "!="   )
    ,(("and",2)   , PDBS $ infixOp "and"  )
    ,(("or",2)    , PDBS $ infixOp "or"   )

    ,(("true",0)  , PDBS $ nullary "True" )
    ,(("false",0) , PDBS $ nullary "False")
    ,(("null",0)  , PDBS $ nullary "None" )

    ,(("-",1)     , PDBS $ call "-"       )
    ,(("!",1)     , PDBS $ call "not"     )
    ,(("not",1)   , PDBS $ call "not"     )
    ,(("mod",1)   , PDBS $ call "mod"     )
    ,(("abs",1)   , PDBS $ call "abs"     )
    ,(("log",1)   , PDBS $ call "log"     )
    ,(("exp",1)   , PDBS $ call "exp"     )
    ,(("eval",1)  , PDBS $ call "None;exec ")
    -- XXX not quite what we want, but something like this might
    -- be nice to have.
    -- ,(("pair",2)  , PDBS $ call ""        )
    ]
 where
  nullary v  _ _   = v
  call    fn _ vis = fn <> (parens $ sepBy "," $ mpv vis)

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


pslice vs = brackets $
       sepBy "," (map (\x -> if nGround (x^.mv_mi) then pretty (x^.mv_var) else ":") vs)
       <> "," -- add a list comma to ensure getitem is always passed a tuple.

filterGround = map (^.mv_var) . filter (not.nGround.(^.mv_mi))

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
            <+> equals
                <+> "peel" <> (parens $ pfas f vs <> comma <> pretty i)
     )
    -- you'll get a "TypeError: 'NoneType' is not iterable."
    `above` "except (TypeError, AssertionError): continue"
pdope_ (OPWrap v vs f) = pretty v
                           <+> equals
                           <+> "build"
                           <> (parens $ pfas f vs <> comma
                                <> (sepBy "," $ map pretty vs))

pdope_ (OPIter v vs f _ (Just (PDBS c))) = pretty (v^.mv_var)
                                     <+> equals
                                     <+> c v vs

pdope_ (OPIter o m f _ Nothing) =
      let mo = m ++ [o] in
          "for" <+> (tupledOrUnderscore $ filterGround mo)
                <+> "in" <+> functorIndirect "chart" f m <> pslice mo <> colon

    -- XXX Ought to make i and vs conditional on... doing debugging or the
    -- aggregator for this head caring.  The latter is a good bit more
    -- advanced than we are right now.
pdope_ (OPEmit h r i vs) =
  "emit" <> tupled [ pretty h
                   , pretty r
                   , pretty i
                   , varmap
                   ]
 where
  -- A python map of variable name to value
  varmap = encloseSep lbrace rbrace comma
         $ map (\v -> let v' = pretty v in dquotes v' <+> colon <+> v') vs

-- | Render a dopamine sequence's checks and loops above a (indended) core.
pdope :: Actions PyDopeBS -> Doc e
pdope _d =         (indent 4 $ "for _ in [None]:")
           `above` (indent 8 $ go _d)
 where
  go []  = empty
  go (x:xs) = let indents = case x of OPIter _ _ _ d _ -> d /= Det ; _ -> False
              in above (pdope_ x)
                 . (if indents then indent 4 else id)
                 $ go xs


printPlanHeader :: Handle -> Rule -> Cost -> Maybe Int -> IO ()
printPlanHeader h r c mn = do
  hPutStrLn h $ "# --"
    -- XXX This "prefixSD" thing is the only real reason we're doing this in
    -- IO; it'd be great if wl-pprint-extras understood how to prefix each
    -- line it was laying out.
  displayIO h $ prefixSD "# " $ renderPretty 1.0 100
                $ (prettySpanLoc $ r_span r) <> line
  hPutStrLn h $ "# EvalIx: " ++ (show mn)
  hPutStrLn h $ "# Cost: " ++ (show c)

printInitializer :: Handle -> Rule -> Actions PyDopeBS -> IO ()
printInitializer fh rule@(Rule _ h _ r _ _ ucruxes _) dope = do
  displayIO fh $ renderPretty 1.0 100
                 $ "@initializer" <> parens (uncurry pfa $ MA.fromJust $ findHeadFA h ucruxes)
                   `above` "def" <+> char '_' <> tupled [] <+> colon
                   `above` pdope dope
                   <> line

-- XXX INDIR EVAL
printUpdate :: Handle -> Rule -> Maybe DFunctAr -> (DVar, DVar) -> Actions PyDopeBS -> IO ()
printUpdate fh rule@(Rule _ h _ r _ _ _ _) (Just (f,a)) (hv,v) dope = do
  displayIO fh $ renderPretty 1.0 100
                 $ "@register" <> parens (pfa f a)
                   `above` "def" <+> char '_' <> tupled (map pretty [hv,v]) <+> colon
                   `above` pdope dope
                   <> line

------------------------------------------------------------------------}}}
-- Driver                                                               {{{

driver :: BackendDriver PyDopeBS
driver am um {-qm-} is fh = do
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
       printPlanHeader fh r c (Just n)
       printUpdate fh r fa (vi,vo) act

  hPutStrLn fh ""
  hPutStrLn fh $ "# ==Initializers=="
  forM_ is $ \(r,c,a) -> do
    printPlanHeader  fh r c Nothing
    printInitializer fh r a

{-
  hPutStrLn fh $ "# ==Queries=="

  forM_ (M.toList qm) $ \(fa, ps) -> do
    hPutStrLn fh $ "# " ++ show fa
    forM_ ps $ \(r,c,qv,a) -> do
      printPlanHeader fh r c
      hPutStrLn fh $ "# " ++ show qv
      -- XXX
      -- displayIO fh $ renderPretty 1.0 100 $ pdope a "XXX"
      hPutStrLn fh ""
-}

------------------------------------------------------------------------}}}
-- Export                                                               {{{

pythonBackend :: Backend
pythonBackend = Backend builtins
                        (M.keysSet constants)
                        (\o is _ _ (PDBS e) -> e o is)
                        driver

------------------------------------------------------------------------}}}
