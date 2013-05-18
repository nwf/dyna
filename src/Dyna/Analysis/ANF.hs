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
-- In short, explicit marks ('ECExplicit') are always obeyed; absent one,
-- ('ECFunctor') the functor's self disposition ('SDQuote' or 'SDEval') is
-- obeyed; if the functor has no preference ('SDInherit'), the outer
-- functor's argument disposition is used as a last resort ('ADQuote' or
-- 'ADEval').  There is, however, one important caveat: /variables/ and
-- /primitive terms/ (e.g.  numerics, strings, literal dynabases, foreign
-- terms, ...) have self dispositions of preferring structural
-- interpretation.  Variables may be meaningfully explicitly evaluated, with
-- the effect of evaluating their bindings.  Attempting to evaluate a
-- primitive is an error.
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

-- FIXME: "str" is the same a constant str.

--     timv: should there ever be more than one side condition? shouldn't it be
--     a single result variable after normalization? I see that if I use comma
--     to combine my conditions I get mutliple variables but should side
--     condtions be combined with comma? I was under the impression that we
--     always want strong Boolean values (i.e. none of that three-values null
--     stuff).
--
--     It would also be nice if spans were killed... maybe there is an argument
--     against this.
--

-- Header material                                                      {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.ANF (
	Crux, EvalCrux(..), UnifCrux(..), cruxIsEval, cruxVars,
	
    Rule(..), ANFAnnots, ANFWarns,
    normTerm, normRule, runNormalize,

	-- * Internals
	SelfDispos(..), ArgDispos(..), ECSrc(..), EvalCtx,

    -- * Placeholders
    evalCruxFA, findHeadFA, r_cruxes,
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
import qualified Dyna.ParserHS.Parser       as P
import           Dyna.Term.TTerm
import           Dyna.Term.Normalized
import           Dyna.Term.SurfaceSyntax
import           Dyna.XXX.DataUtils (mapInOrApp)
-- import           Dyna.Test.Trifecta         -- XXX
import qualified Text.Trifecta              as T

------------------------------------------------------------------------}}}
-- Preliminaries                                                        {{{

data ECSrc = ECFunctor
           | ECExplicit

type EvalCtx = (ECSrc,ArgDispos)

type ANFAnnots = M.Map DVar [Annotation (T.Spanned P.Term)]
type ANFWarns  = [(BU.ByteString, [T.Span])]

newtype ANFDict = AD { ad_dt :: DisposTab }
{-
  { -- | A map from (functor,arity) to a list of bits indicating whether to
    -- (True) or not to (False) evaluate that positional argument.
    --
    -- XXX This isn't going to work when we get more complicated terms.
    --
    -- XXX Stronger type desired: we'd like static assurance that the
    -- length of the list matches the arity in the key!
    ad_arg_dispos  :: (DFunct,Int) -> [ArgDispos]

    -- | A map from (functor,arity) to self disposition.
  , ad_self_dispos :: (DFunct,Int) -> SelfDispos
  }
-}

mergeDispositions :: SelfDispos -> (ECSrc, ArgDispos) -> ArgDispos
mergeDispositions = md
 where
  md SDInherit (_,d)                = d
  md SDEval    (ECExplicit,ADQuote) = ADQuote
  md SDEval    (_,_)                = ADEval
  md SDQuote   (ECExplicit,ADEval)  = ADEval
  md SDQuote   (_,_)                = ADQuote

------------------------------------------------------------------------}}}
-- Cruxes                                                               {{{

data EvalCrux v = CCall v [v] DFunct
                | CEval v v
 deriving (Eq,Ord,Show)

data UnifCrux v n = CStruct v [v] DFunct   -- Known structure building
                  | CAssign v n            -- Constant loading
                  | CEquals v v            -- Equality constraint
                  | CNotEqu v v            -- Disequality constraint
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

newEval :: (MonadState ANFState m) => String -> EVF -> m DVar
newEval pfx t = do
    n   <- nextVar pfx
    ne  <- as_next_eval <<%= (+1)
    as_ecruxes %= IM.insert ne (either (CEval n)
                                       (uncurry (flip (CCall n))) t)
    return n

newAssign :: (MonadState ANFState m) => String -> ENF -> m DVar
newAssign pfx t =
  case t of
    Left (NTVar  v) -> return v
    Left (NTBase b) -> go (Left  b)
    Right u         -> go (Right u)
 where
  go u = do
    n   <- nextVar pfx
    addUCrux (either (CAssign n) (uncurry (flip (CStruct n))) u)
    return n

newAnnot :: (MonadState ANFState m)
         => DVar -> Annotation (T.Spanned P.Term) -> m ()
newAnnot v a = as_annot %= mapInOrApp v a

{-
newAssignNT :: (MonadState ANFState m) => String -> NTV -> m DVar
newAssignNT _   (NTVar x)     = return x
newAssignNT pfx x             = newAssign pfx $ Left x
-}

doUnif :: (MonadState ANFState m) => DVar -> DVar -> m ()
doUnif v w = if v == w
              then return ()
              else addUCrux (CEquals v w)

newWarn :: (MonadState ANFState m) => B.ByteString -> [T.Span] -> m ()
newWarn msg loc = as_warns %= ((msg,loc):)

------------------------------------------------------------------------}}}
-- Normalize a Term                                                     {{{

-- | Convert a syntactic term into ANF; while here, move to a
-- flattened representation.
--
-- The ANFState ensures that variables are unique; we additionally give them
-- \"semi-meaningful\" prefixes, but these should not be relied upon for
-- anything actually meaningful (but they serve as great debugging aids!).
-- While here, we stick a prefix on user variables to ensure that they are
-- disjoint from the variables we generate and use internally.
--
-- XXX This sheds span information entirely, which is probably not what we
-- actually want.  Note that we're careful to keep a stack of contexts
-- around, so we should probably do something clever like attach them to
-- operations we extract?
normTerm_ :: (Functor m, MonadState ANFState m, MonadReader ANFDict m)
               => EvalCtx       -- ^ In an evaluation context?
               -> [T.Span]      -- ^ List of spans traversed
               -> P.Term        -- ^ Term being digested
               -> m NTV

-- Variables only evaluate in explicit context
--
-- While here, replace bare underscores with unique names and rename all
-- remaining user variables to ensure that they do not collide with internal
-- names.
--
-- XXX is this the right place for that?
normTerm_ c _ (P.TVar v) = do
    v' <- if v == "_" then nextVar "_w" else return (BC.cons 'u' v)
    case c of
       (ECExplicit,ADEval) -> NTVar `fmap` newEval "_v" (Left v')
       _                   -> return $ NTVar v'

-- Numerics get returned in-place and raise a warning if they are evaluated.
normTerm_ c   ss  (P.TBase x@(TNumeric _)) = do
    case c of
      (ECExplicit,ADEval) -> newWarn "Ignoring request to evaluate numeric" ss
      _                   -> return ()
    return $ NTBase x

-- Strings too
normTerm_ c   ss  (P.TBase x@(TString _))  = do
    case c of
      (ECExplicit,ADEval) -> newWarn "Ignoring request to evaluate string" ss
      _                   -> return ()
    return $ NTBase x

-- Annotations
--
-- XXX this is probably the wrong thing to do
normTerm_ c   ss (P.TAnnot a (t T.:~ st)) = do
    v <- normTerm_ c (st:ss) t >>= newAssign "_a" . Left
    newAnnot v a
    return $ NTVar v

-- Quote makes the context explicitly a quoting one
normTerm_ _   ss (P.TFunctor f [t T.:~ st]) | f == dynaQuoteOper = do
    normTerm_ (ECExplicit,ADQuote) (st:ss) t

-- Evaluation is a little different: in addition to forcing the context to
-- evaluate, it must also evaluate if the context from on high is one of
-- evaluation!
normTerm_ c   ss (P.TFunctor f [t T.:~ st]) | f == dynaEvalOper =
    normTerm_ (ECExplicit,ADEval) (st:ss) t
    >>= \nt -> case c of
                (_,ADEval) -> case nt of
                                NTVar  v -> NTVar `fmap` newEval "_s" (Left v)
                                NTBase b -> do
                                             newWarn "Ignoring deref of literal" ss
                                             return $ NTBase b
                _          -> return nt

-- "is/2" is sort of exciting.  We normalize the second argument in an
-- evaluation context and the first in a quoted context.  Then, if the
-- result is quoted, we simply build up some structure.  If it's evaluated,
-- on the other hand, we reduce it to a unification of these two pieces and
-- return (XXX what ought to be) a unit.
normTerm_ c   ss (P.TFunctor f [x T.:~ sx, v T.:~ sv]) | f == dynaEvalAssignOper = do
    nx <- normTerm_ (ECFunctor, ADQuote) (sx:ss) x >>= newAssign "_i" . Left
    nv <- normTerm_ (ECFunctor, ADEval ) (sv:ss) v >>= newAssign "_i" . Left
    case c of
        (_,ADEval) -> do
                       _ <- doUnif nx nv
                       NTVar `fmap` newAssign "_i" (Right (dynaUnitTerm,[]))
        _          -> do
                       NTVar `fmap` newAssign "_i" (Right (dynaEvalAssignOper,[nx,nv]))

-- ",/2", "whenever/2", and "for/2" are also reserved words of the language
-- and get handled here.
--
-- XXX This is wrong, too, of course; these should really be moved into a
-- standard prelude.  But there's no facility for that right now and no
-- reason to make the backend know about them since that's also wrong!
--
-- XXX XREF:ANFRESERVED
normTerm_ (_,ADEval) ss (P.TFunctor f [i T.:~ si, r T.:~ sr]) | f == dynaConjOper = do
    ni <- normTerm_ (ECFunctor, ADEval) (si:ss) i >>= newAssign "_e" . Left
    nv <- normTerm_ (ECFunctor, ADEval) (sr:ss) r >>= newAssign "_e" . Left

    t' <- newAssign "_e" (Right (dynaUnitTerm,[]))
    _  <- doUnif ni t'
    return $ NTVar nv

normTerm_ c@(_,ADEval) ss (P.TFunctor f [sr, si]) | f `elem` dynaRevConjOpers =
    normTerm_ c ss (P.TFunctor dynaConjOper [si,sr])

-- Functors have both top-down and bottom-up dispositions on
-- their handling.
normTerm_ c   ss (P.TFunctor f as) = do

    argdispos <- asks $ flip fArgEvalDispos (f,length as) . ad_dt
    normas <- mapM (\(a T.:~ s,d) -> normTerm_ (ECFunctor,d) (s:ss) a)
                   (zip as argdispos)

    -- Convert everything to DVars and, while here, do a linearization
    -- pass to strip duplicate vars out.  We need pattern matching to be
    -- linear-with-checks in later pipeline stages so that we can, for
    -- example, correctly reject updates that are not the right shape.
    normas' <- let delin (vs,r) x =
                     case x of
                       NTVar v | not (v `elem` vs) -> do
                            return (v:vs,v:r)
                       NTVar v -> do
                            v' <- nextVar "_x"
                            doUnif v v'
                            return (vs,v':r)
                       _ -> do
                            v' <- newAssign "_x" (Left x)
                            return (vs,v':r)
               in (reverse . snd) `fmap` foldM delin ([],[]) normas

    selfdispos <- asks $ flip fSelfEvalDispos (f,length as) . ad_dt

    let dispos = mergeDispositions selfdispos c

    fmap NTVar $
     case dispos of
       ADEval  -> newEval "_f" . Right
       ADQuote -> newAssign "_u" . Right
      $ (f,normas')

normTerm :: (Functor m, MonadState ANFState m, MonadReader ANFDict m)
         => Bool               -- ^ In an evaluation context?
         -> T.Spanned P.Term   -- ^ Term to digest
         -> m NTV
normTerm c (t T.:~ s) = normTerm_ (ECFunctor,if c then ADEval else ADQuote)
                                  [s] t

------------------------------------------------------------------------}}}
-- Normalize a Rule                                                     {{{

data Rule = Rule { r_index      :: Int
                 , r_head       :: DVar
                 , r_aggregator :: DAgg
                 , r_result     :: DVar
                 , r_span       :: T.Span
                 , r_annots     :: ANFAnnots
                 , r_ucruxes    :: S.Set (UnifCrux DVar TBase)
                 , r_ecruxes    :: IM.IntMap (EvalCrux DVar)
                 }
 deriving (Show)


normRule :: T.Spanned P.Rule   -- ^ Term to digest
         -> (Rule, ANFWarns)
normRule (P.Rule i h a r dt T.:~ sp) = 
  let (ru,s) = runNormalize dt $ do
               nh  <- normTerm False h >>= newAssign "_h" . Left
               nr  <- normTerm True  r >>= newAssign "_r" . Left
               return $ Rule i nh a nr sp
  in (ru (s^.as_annot) (s^.as_ucruxes) (s^.as_ecruxes),s^.as_warns)

------------------------------------------------------------------------}}}
-- Run the normalizer                                                   {{{

-- | Run the normalization routine.
--
-- Use as @runNormalize nRule@
runNormalize :: DisposTab
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

------------------------------------------------------------------------}}}
