---------------------------------------------------------------------------
-- | Some simple analysis to move to ANF.
--
-- In Dyna's surface syntax, there exists both \"in-place evaluation\" and
-- \"in-place construction\".  How do we deal with this?  Well, it's a
-- little messy.
--
--   1. There are explicit \"eval\" (@*@) and \"quote\" (@&@) operators
--   which may be used to manually specify which is intended.
--
--   2. Functors specify \"argument dispositions\", indicating whether they
--   prefer to evaluate or build structure in each argument position.
--
--   3. Functors further specify \"self disposition\", indicating whether
--   they 1) leave the decision to the parent, 2) prefer to build structure
--   unless explicitly evaluated, or 3) prefer to be evaluated unless
--   explicitly quoted.
--
-- Note that in rules, the head is by default not evaluated (regardless of
-- the disposition of their outer functor), while the body is interpreted as
-- a term expression (or list of term expressions) to be evaluated.
--
-- XXX This is really quite simplistic and is probably a far cry from where
-- we need to end up.  Especially of note is that we do not yet parse any
-- sort of pragmas for augmenting our disposition list.
--
-- XXX The handling for \"is/2\" is probably wrong, but differently wrong than
-- before, at least.
--
-- XXX We really should do some CSE/GVN somewhere right after this pass, but
-- be careful about linearity!
--
-- XXX Maybe we should be doing something differently for the head variable
-- of the ANF -- we know (or should know, anyway) that it's either the
-- result of evaluation (in the tricky examples like @*f += 1@) or a
-- structured term.  None of our as_* fields give us that guarantee.  See
-- "Dyna.Backend.Python"'s @findHeadFA@ function.

-- XXX This module does not use Control.Lens but should.
--
-- XXX The handling of underscores is not quite right and frequently leads
-- to dead assignments.
--
-- XXX This rewrite flattens the distinction the parser is careful to keep
-- for DTPosn.  This is bad.

-- FIXME: "str" is the same a constant str.

-- Header material                                                      {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.ANF (
	Crux, EvalCrux(..), UnifCrux(..), cruxIsEval, cruxVars, allCruxVars,
	
    Rule(..), RuleIx, ANFAnnots, ANFWarns,
    normTerm, normRule, runNormalize,

	-- * Internals
	SelfDispos(..), ArgDispos(..), EvalMarks,

    -- * Placeholders
    evalCruxFA, findHeadFA, r_cruxes, extractHeadVars
) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
-- import           Control.Unification
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.UTF8       as BU
import qualified Data.ByteString            as B
-- import qualified Data.Char                  as C
import qualified Data.Map                   as M
import qualified Data.Maybe                 as MA
import qualified Data.IntMap                as IM
import qualified Data.Set                   as S
-- import qualified Debug.Trace                as XT
import           Dyna.Main.Defns
import           Dyna.Main.Exception
import           Dyna.ParserHS.SyntaxTheory
import           Dyna.ParserHS.Syntax
import qualified Dyna.ParserHS.Types        as P
import           Dyna.Term.TTerm
import           Dyna.Term.Normalized
import           Dyna.XXX.DataUtils (mapInOrCons, zipWithTails)
import           Dyna.XXX.MonadUtils (timesM)
-- import           Dyna.Test.Trifecta         -- XXX
import qualified Text.Trifecta              as T

------------------------------------------------------------------------}}}
-- Preliminaries                                                        {{{

type EvalMarks = (Int,Bool)

type ANFAnnots = M.Map DVar [Annotation (T.Spanned P.Term)]
type ANFWarns  = [(BU.ByteString, [T.Span])]

newtype ANFDict = AD { ad_dt :: DisposTab DFunctAr }

mergeDispositions :: EvalMarks -> SelfDispos -> ArgDispos -> Int
mergeDispositions = md
 where
  md (0,True)  _         _       = 0    -- Explicit "&"
  md (0,False) SDQuote   _       = 0    -- No marks, self-quote
  md (0,False) SDInherit ADQuote = 0    -- No marks, inherit quoted
  md (0,False) SDInherit ADEval  = 1    -- No marks, inherit eval
  md (0,False) SDEval    _       = 1    -- No marks, self-eval
  md (n,True)  _         _       = n    -- n *s followed by &
  md (n,False) SDEval    _       = n+1  -- n *s, self-eval
  md (n,False) _         _       = n    -- n *s, self-quote or eval

------------------------------------------------------------------------}}}
-- Cruxes                                                               {{{

data EvalCrux v = CCall v [v] DFunct       -- ^ Known structure evaluation
                | CEval v v                -- ^ Indirect evaluation
 deriving (Eq,Ord,Show)

data UnifCrux v n = CStruct v [v] DFunct   -- ^ Known structure building
                  | CAssign v n            -- ^ Constant loading
                  | CEquals v v            -- ^ Equality constraint
                  | CNotEqu v v            -- ^ Disequality constraint
 deriving (Eq,Ord,Show)

type Crux v n = Either (Int,EvalCrux v) (UnifCrux v n)

cruxIsEval :: Crux v n -> Bool
cruxIsEval (Left _) = True
cruxIsEval (Right _) = False

cruxVars :: Crux DVar TBase -> S.Set DVar
cruxVars = either evalVars unifVars
 where
  evalVars (_,cr) = case cr of
    CCall   o is _ -> S.fromList (o:is)
    CEval   o i    -> S.fromList [o,i]
  unifVars cr = case cr of
    CStruct o is _ -> S.fromList (o:is)
    CAssign o _    -> S.singleton o
    CEquals o i    -> S.fromList [o,i]
    CNotEqu o i    -> S.fromList [o,i]

allCruxVars :: S.Set (Crux DVar TBase) -> S.Set DVar
allCruxVars = S.unions . map cruxVars . S.toList


------------------------------------------------------------------------}}}
-- ANF State                                                            {{{

data ANFState = AS
              { _as_next_var  :: !Int
              , _as_next_eval :: !Int
              , _as_ucruxes   :: S.Set (UnifCrux DVar TBase)
              , _as_ecruxes   :: IM.IntMap (EvalCrux DVar)
              , _as_annot :: ANFAnnots
              , _as_warns :: ANFWarns
              }
 deriving (Show)
$(makeLenses ''ANFState)

addUCrux :: (MonadState ANFState m) => UnifCrux DVar TBase -> m ()
addUCrux c = as_ucruxes %= (S.insert c)

nextVar :: (MonadState ANFState m) => String -> m DVar
nextVar pfx = do
    vn  <- as_next_var <<%= (+1)
    return $ BU.fromString $ pfx ++ show vn

mkFromUVar :: (MonadState ANFState m) => B.ByteString -> m B.ByteString
mkFromUVar v = if v == "_" then nextVar "_w" else return (BC.cons 'u' v)

doEval :: (MonadState ANFState m) => EVF -> DVar -> m ()
doEval t n = do
    ne  <- as_next_eval <<%= (+1)
    as_ecruxes %= IM.insert ne (either (CEval n)
                                       (uncurry (flip (CCall n))) t)

newEval :: (MonadState ANFState m) => String -> EVF -> m DVar
newEval pfx t = do
    n   <- nextVar pfx
    doEval t n
    return n

doLoadBase :: (MonadState ANFState m) => TBase -> DVar -> m ()
doLoadBase n v = addUCrux (CAssign v n)

newLoad :: (MonadState ANFState m) => String -> ENF -> m DVar
newLoad pfx t =
  case t of
    Left (NTVar  v) -> return v
    Left (NTBase b) -> go (Left  b)
    Right u         -> go (Right u)
 where
  go u = do
    n   <- nextVar pfx
    addUCrux (either (CAssign n) (uncurry (flip (CStruct n))) u)
    return n

doStruct :: (MonadState ANFState m) => FDT -> DVar -> m ()
doStruct (f,vs) v = addUCrux (CStruct v vs f)

doUnif :: (MonadState ANFState m) => DVar -> DVar -> m ()
doUnif v w = if v == w
              then return ()
              else addUCrux (CEquals v w)

doAnnot :: (MonadState ANFState m)
         => Annotation (T.Spanned P.Term) -> DVar -> m ()
doAnnot a v = as_annot %= mapInOrCons v a

newWarn :: (MonadState ANFState m) => B.ByteString -> [T.Span] -> m ()
newWarn msg loc = as_warns %= ((msg,loc):)

------------------------------------------------------------------------}}}
-- Normalize a Term                                                     {{{

-- | Convert a syntactic term into ANF; while here, move to a flattened
-- representation.
--
-- The ANFState ensures that variables are unique; we additionally give them
-- \"meaningful\" prefixes, but these should not be relied upon for
-- anything actually meaningful (but they serve as great debugging aids!).
-- While here, we stick a prefix on user variables to ensure that they are
-- disjoint from the variables we generate and use internally.
--
-- XXX This sheds span information entirely, except in the case of warnings,
-- which is probably not what we actually want.  Note that we're careful to
-- keep a stack of contexts around, so we should probably do something
-- clever like attach them to operations we extract?
normTerm_ :: (Functor m, MonadState ANFState m, MonadReader ANFDict m)
               => ArgDispos     -- ^ The disposition of the outermost context
               -> EvalMarks     -- ^ Evaluation marks accumulated
               -> [T.Span]      -- ^ List of spans traversed
               -> Maybe DVar    -- ^ Destination, if present
               -> P.Term        -- ^ Term being digested
               -> m ()

-- Variables only evaluate in explicit context
--
-- While here, replace bare underscores with unique names and rename all
-- remaining user variables to ensure that they do not collide with internal
-- names.
--
-- XXX is this the right place for that?
normTerm_ a m _ d (P.Term (P.TVar v)) = do
  v' <- mkFromUVar v
  v'' <- timesM (newEval "_v" . Left) (mergeDispositions m SDQuote a) v'
  maybe (return ()) (doUnif v'') d

-- Numerics get returned in-place and raise a warning if they are evaluated.
normTerm_ _ m ss d (P.Term (P.TPrim x@(TNumeric _))) = do
  case m of
    (_,True)  -> newWarn "Suppressing numeric evaluation is unnecessary" ss
    (0,False) -> return ()
    (_,False) -> newWarn "Ignoring request to evaluate numeric" ss
  maybe (newWarn "Numeric literal is discarded" ss)
        (doLoadBase x)
        d

-- Strings too
normTerm_ _ m ss d (P.Term (P.TPrim x@(TString _)))  = do
  case m of
    (_,True)  -> newWarn "Suppressing string evaluation is unnecessary" ss
    (0,False) -> return ()
    (_,False) -> newWarn "Ignoring request to evaluate string" ss
  maybe (newWarn "String literal is discarded" ss)
        (doLoadBase x)
        d

-- Booleans too
normTerm_ _ m ss d (P.Term (P.TPrim x@(TBool _))) = do
  case m of
    (_,True)  -> newWarn "Suppressing boolean evaluation is unnecessary" ss
    (0,False) -> return ()
    (_,False) -> newWarn "Ignoring request to evaluate boolean" ss
  maybe (newWarn "Boolean literal is discarded" ss)
        (doLoadBase x)
        d

-- "is/2" is sort of exciting.  We normalize the second argument in an
-- evaluation context and the first in a quoted context.  Then, if the
-- result is quoted, we simply build up some structure.  If it's evaluated,
-- on the other hand, we reduce it to a unification of these two pieces and
-- return (XXX what ought to be) a unit.
normTerm_ a m ss d (P.Term (P.TInfix f (x T.:~ sx) (v T.:~ sv)))
    | f == dynaEvalAssignOper = do
  nx <- nextVar "_i"
  normTerm_ ADQuote (0,False) (sx:ss) (Just nx) x

  nv <- nextVar "_j"
  normTerm_ ADEval  (0,False) (sv:ss) (Just nv) v
  
  case (d, mergeDispositions m SDInherit a) of
    (Nothing, 0) -> newWarn "Quoted functor discarded" ss
    (Just d', 0) -> doStruct (dynaEvalAssignOper,[nx,nv]) d'
    (Nothing, 1) -> doUnif nx nv
    (_      , n) -> do
                     _ <- doUnif nx nv
                     t <- newLoad "_x" (Left $ NTBase dynaUnitTerm)
                     r <- timesM (newEval "_x" . Left) (n-1) t
                     maybe (return ()) (doUnif r) d

-- ",/2", "whenever/2", and "for/2" are also reserved words of the language
-- and get handled here.
--
-- XXX This is wrong, too, of course; these should really be moved into a
-- standard prelude.  But there's no facility for that right now and no
-- reason to make the backend know about them since that's also wrong!
--
-- XXX XREF:ANFRESERVED
normTerm_ a m ss d (P.Term (P.TInfix f (i T.:~ si) (r T.:~ sr)))
    | f == dynaConjOper =
  normConjunct ss f i si r sr (mergeDispositions m SDInherit a) d False

normTerm_ a m ss d (P.Term (P.TInfix f (r T.:~ sr) (i T.:~ si)))
    | f `elem` dynaRevConjOpers =
  normConjunct ss f i si r sr (mergeDispositions m SDInherit a) d True

-- Annotations
--
-- XXX this is probably the wrong thing to do
normTerm_ a m ss d (P.Term (P.TAnnot an (t T.:~ st))) = do
    normTerm_ a m (st:ss) d t
    maybe (newWarn "Annotation discarded" ss)
          (doAnnot an)
          d

-- Quote makes the context explicitly a quoting one
normTerm_ a (n,q) ss d (P.Term (P.TPrefix f (t T.:~ st)))
    | f == dynaQuoteOper = do
  when q $ newWarn "Superfluous quotation marker" ss
  normTerm_ a (n,True) (st:ss) d t

-- Evaluation just bumps the number of evaluations and resets the quoted
-- flag to False
normTerm_ a (n,_) ss d (P.Term (P.TPrefix f (t T.:~ st)))
    | f == dynaEvalOper =
  normTerm_ a (n+1,False) (st:ss) d t

normTerm_ ctx m ss d (P.Term (P.TPrefix f (a T.:~ st))) = do
  (sd, [ad]) <- asks $ ($ ((f,1),DTPPrefix)) . ad_dt
  v <- nextVar "_r"
  normTerm_ ad (0,False) (st:ss) (Just v) a
  normFunctResolve ss d m sd ctx f [v]

normTerm_ ctx m ss d (P.Term (P.TPostfix f (a T.:~ st))) = do
  (sd, [ad]) <- asks $ ($ ((f,1),DTPPostfix)) . ad_dt
  v <- nextVar "_o"
  normTerm_ ad (0,False) (st:ss) (Just v) a
  normFunctResolve ss d m sd ctx f [v]

normTerm_ ctx m ss d (P.Term (P.TInfix f (a T.:~ sa) (b T.:~ sb))) = do
  (sd, [ada,adb]) <- asks $ ($ ((f,2),DTPInfix)) . ad_dt
  va <- nextVar "_i"
  normTerm_ ada (0,False) (sa:ss) (Just va) a
  vb <- nextVar "_i"
  normTerm_ adb (0,False) (sb:ss) (Just vb) b
  normFunctResolve ss d m sd ctx f [va,vb]

-- Ah, the "boring" case of functors!
normTerm_ ctx m ss d (P.Term (P.TFunctor f as)) = do
  -- Look up disposition information
  (sd, ads) <- asks $ ($ ((f,length as),DTPFunctor)) . ad_dt

  -- Conjure up destinations for all arguments, trying to preserve the
  -- original variables here if we can, but doing a linearization
  -- pass while we're at it.
  argvars <- flip evalStateT S.empty $ forM as $ \a -> do
               already <- get
               case a of
                 P.Term (P.TVar avv) T.:~ _
                   | not (avv `S.member` already)
                   -> modify (S.insert avv) >> lift (mkFromUVar avv)
                 _ -> lift (nextVar "_a")

  -- Normalize all arguments appropriately
  mapM_ (\(a T.:~ s,(v,c)) -> normTerm_ c (0,False) (s:ss) (Just v) a)
        ( zipWithTails (,) panic panic as
          $ zipWithTails (,) panic panic argvars ads)
 
  -- And bring everything together
  normFunctResolve ss d m sd ctx f argvars
 
 where
  panic = dynacPanic "Length mismatch in disposition table while normalizing"

normFunctResolve ss d m sd ctx f avs =
   case (mergeDispositions m sd ctx, d) of
    (0,Nothing) -> newWarn "Quoted functor discarded" ss
    (0,Just d') -> doStruct (f,avs) d'
    (1,Just d') -> doEval (Right (f,avs)) d'
    (n,_      ) -> do
                    t  <- newEval "_e" (Right (f,avs))
                    ct <- timesM (newEval "_x" . Left) (n-1) t
                    maybe (return ()) (doUnif ct) d


normConjunct :: (Functor m, MonadReader ANFDict m, MonadState ANFState m)
             => [T.Span]
             -> DFunct
             -> P.Term -> T.Span -> P.Term -> T.Span
             -> Int
             -> Maybe DVar
             -> Bool
             -> m ()
normConjunct ss f i si r sr n d rev =
  case (n,d) of
    (0,Nothing) -> do
                    di <- nextVar "_b"
                    dr <- nextVar "_c"
                    go di dr
                    newWarn "Quoted functor discarded" ss
    (0,Just d') -> do
                    di <- nextVar "_b"
                    dr <- nextVar "_c"
                    go di dr
                    doStruct (selfstruct di dr) d'
    (1,Just d') -> do
                    di <- newLoad "_b" (Left $ NTBase dynaUnitTerm)
                    go di d'
    (_,_      ) -> do
                    di <- newLoad "_b" (Left $ NTBase dynaUnitTerm)
                    dr <- nextVar "_c"
                    go di dr
                    ct <- timesM (newEval "_x" . Left) (n-1) dr
                    maybe (return ()) (doUnif ct) d


 where
  selfstruct ni nr = (f,if rev then [nr,ni] else [ni,nr]) 

  go di dr = do
    normTerm_ ADEval (0,False) (si:ss) (Just di) i
    normTerm_ ADEval (0,False) (sr:ss) (Just dr) r


normTerm :: (Functor m, MonadState ANFState m, MonadReader ANFDict m)
         => ArgDispos          -- ^ In an evaluation context?
         -> T.Spanned P.Term   -- ^ Term to digest
         -> m DVar
normTerm a (t T.:~ s) = do
  v <- nextVar "_t"
  normTerm_ a (0,False) [s] (Just v) t
  return v

------------------------------------------------------------------------}}}
-- Normalize a Rule                                                     {{{

data Rule = Rule { r_index      :: RuleIx
                 , r_head       :: DVar
                 , r_aggregator :: DAgg
                 , r_result     :: DVar
                 , r_span       :: T.Span
                 , r_annots     :: ANFAnnots
                 , r_ucruxes    :: S.Set (UnifCrux DVar TBase)
                 , r_ecruxes    :: IM.IntMap (EvalCrux DVar)
                 }
 deriving (Show)


normRule :: (RuleIx, DisposTab DFunctAr, T.Spanned P.Rule)   -- ^ Rule to digest
         -> (Rule, ANFWarns)
normRule (i, dt, P.Rule h a r T.:~ sp) =
  let (ru,s) = runNormalize dt $ do
               nh  <- normTerm ADQuote h
               nr  <- normTerm ADEval  r
               return $ Rule i nh a nr sp
  in (ru (s^.as_annot) (s^.as_ucruxes) (s^.as_ecruxes),s^.as_warns)

------------------------------------------------------------------------}}}
-- Run the normalizer                                                   {{{

-- | Run the normalization routine.
--
-- Use as @runNormalize nRule@
runNormalize :: DisposTab DFunctAr
             -> ReaderT ANFDict (State ANFState) a -> (a, ANFState)
runNormalize dt =
  flip runState   (AS 0 0 S.empty IM.empty M.empty []) .
  flip runReaderT (AD dt)

------------------------------------------------------------------------}}}
-- Placeholders XXX                                                     {{{

r_cruxes :: Rule -> S.Set (Crux DVar TBase)
r_cruxes r = S.union (S.map Right $ r_ucruxes r)
                     (S.map Left $ S.fromList $ IM.assocs $ r_ecruxes r)

evalCruxFA :: EvalCrux DVar -> Maybe DFunctAr
evalCruxFA (CEval _ _) = Nothing
evalCruxFA (CCall _ is f) = Just $ (f, length is)

-- XXX This is terrible and should be replaced with whatever type-checking
-- work we do.
findHeadFA :: DVar -> S.Set (UnifCrux DVar TBase) -> Maybe DFunctAr
findHeadFA h crs = MA.listToMaybe
                 $ MA.mapMaybe m
                 $ S.toList crs
 where
  m (CStruct o is f) | o == h = Just (f,length is)
  m _                         = Nothing

-- XXX There ought to be something better we could do here, possibly
-- involving unification.  This is not very robust to changes.
extractHeadVars :: Rule -> Maybe (DFunct,[DVar])
extractHeadVars (Rule { r_head = h
                      , r_ucruxes = us }) =
     let hbuilds = MA.mapMaybe hs $ S.toList us
      in case hbuilds of
           [] -> Nothing
           y:_ -> Just y
 where
  hs (CStruct v vs f) = if h == v then Just (f,vs) else Nothing
  hs (CAssign _ _   ) = Nothing
  hs (CEquals _ _   ) = Nothing
  hs (CNotEqu _ _   ) = Nothing

------------------------------------------------------------------------}}}
