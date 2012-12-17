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
-- In short, explicit marks are always obeyed; absent one, the functor's
-- self disposition is obeyed; if the functor has no preference, the outer
-- functor's argument disposition is used as a last resort.  There is,
-- however, one important caveat: /variables/ and /primitive terms/ (e.g.
-- numerics, strings, literal dynabases, foreign terms, ...) have self
-- dispositions of preferring structural interpretation.  Variables may be
-- meaningfully explicitly evaluated, with the effect of evaluating their
-- bindings.  Attempting to evaluate a primitive is an error.
--
-- Note that in rules, the head is by default not evaluated (regardless of
-- the disposition of their outer functor), while the body is interpreted as
-- a term expression (or list of term expressions) to be evaluated.
--
-- XXX This is really quite simplistic and is probably a far cry from where
-- we need to end up.  Especially of note is that we do not yet parse any
-- sort of pragmas for augmenting our disposition list.
--
-- XXX The handling for "is/2" is probably wrong, but differently wrong than
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

-- FIXME: "str" is the same a constant str.

-- TODO: ANF Normalizer should return *flat terms* so that we have type-safety
-- can a lint checker can verify we have exhaustive pattern matching... etc.

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
{-# LANGUAGE TupleSections #-}

module Dyna.Analysis.ANF (
    ANFState(..),  FRule(..),
    normTerm, normRule, runNormalize, printANF
) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Unification
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.UTF8       as BU
import qualified Data.ByteString            as B
import qualified Data.Char                  as C
import qualified Data.Map                   as M
import qualified Dyna.ParserHS.Parser       as P
import           Dyna.Analysis.Base
import           Dyna.Term.TTerm
import           Dyna.XXX.DataUtils (mapInOrApp)
import           Dyna.XXX.PPrint (valign)
-- import           Dyna.Test.Trifecta         -- XXX
import           Text.PrettyPrint.Free
import qualified Text.Trifecta              as T


import           Dyna.XXX.Trifecta (prettySpanLoc)

------------------------------------------------------------------------}}}
-- Preliminaries                                                        {{{

data SelfDispos = SDInherit
                | SDEval
                | SDQuote

data ArgDispos = ADEval
               | ADQuote

data ECSrc = ECFunctor
           | ECExplicit

type EvalCtx = (ECSrc,ArgDispos)

data ANFDict = AD
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

mergeDispositions :: SelfDispos -> (ECSrc, ArgDispos) -> ArgDispos
mergeDispositions = md
 where
  md SDInherit (_,d)                = d
  md SDEval    (ECExplicit,ADQuote) = ADQuote
  md SDEval    (_,_)                = ADEval
  md SDQuote   (ECExplicit,ADEval)  = ADEval
  md SDQuote   (_,_)                = ADQuote

data ANFState = AS
              { as_next  :: !Int
              , as_evals :: M.Map DVar EVF
              , as_assgn :: M.Map DVar ENF
              , as_unifs :: [(DVar,DVar)]
              , as_annot :: M.Map DVar [Annotation (T.Spanned P.Term)]
              , as_warns :: [(B.ByteString, [T.Span])]
              }
 deriving (Show)

nextVar :: (MonadState ANFState m) => String -> m DVar
nextVar pfx = do
    vn  <- gets as_next
    modify (\s -> s { as_next = vn + 1 })
    return $ BU.fromString $ pfx ++ show vn

newEval :: (MonadState ANFState m) => String -> EVF -> m DVar
newEval pfx t = do
    n   <- nextVar pfx
    evs <- gets as_evals
    modify (\s -> s { as_evals = M.insert n t evs })
    return n

newAssign :: (MonadState ANFState m) => String -> ENF -> m DVar
newAssign pfx t = do
    n   <- nextVar pfx
    uns <- gets as_assgn
    modify (\s -> s { as_assgn = M.insert n t uns })
    return n

newAnnot :: (MonadState ANFState m)
         => DVar -> Annotation (T.Spanned P.Term) -> m ()
newAnnot v a = do
    modify (\s -> s { as_annot = mapInOrApp v a (as_annot s) })

newAssignNT :: (MonadState ANFState m) => String -> NTV -> m DVar
newAssignNT _   (NTVar x)     = return x
newAssignNT pfx (NTString x)  = newAssign pfx (Left $ NTString x)
newAssignNT pfx (NTNumeric x) = newAssign pfx (Left $ NTNumeric x)

doUnif :: (MonadState ANFState m) => DVar -> DVar -> m ()
doUnif v w = if v == w
              then return ()
              else modify (\s -> s { as_unifs = (v,w):(as_unifs s) })

newWarn :: (MonadState ANFState m) => B.ByteString -> [T.Span] -> m ()
newWarn msg loc = modify (\s -> s { as_warns = (msg,loc):(as_warns s) })

------------------------------------------------------------------------}}}
-- Disposition computations                                             {{{

-- XXX These should be read from declarations
dynaFunctorArgDispositions :: (DFunct, Int) -> [ArgDispos]
dynaFunctorArgDispositions x = case x of
    -- evaluate arithmetic / math
    ("exp", 1) -> [ADEval]
    ("log", 1) -> [ADEval]
    -- logic
    ("and", 2) -> [ADEval, ADEval]
    ("or", 2)  -> [ADEval, ADEval]
    ("not", 1) -> [ADEval]
    (name, arity) ->
       -- If it starts with a nonalpha, it prefers to evaluate arguments
       let d = if C.isAlphaNum $ head $ BU.toString name
                then ADQuote
                else ADEval
       in take arity $ repeat $ d

-- XXX These should be read from declarations
dynaFunctorSelfDispositions :: (DFunct,Int) -> SelfDispos
dynaFunctorSelfDispositions x = case x of
    ("pair",2)   -> SDQuote
    (name, _) ->
       -- If it starts with a nonalpha, it prefers to evaluate
       let d = if C.isAlphaNum $ head $ BU.toString name
                then SDInherit
                else SDEval
       in d

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
normTerm_ c   ss  (P.TNumeric n)    = do
    case c of
      (ECExplicit,ADEval) -> newWarn "Ignoring request to evaluate numeric" ss
      _                   -> return ()
    return $ NTNumeric n

-- Strings too
normTerm_ c   ss  (P.TString s)    = do
    case c of
      (ECExplicit,ADEval) -> newWarn "Ignoring request to evaluate string" ss
      _                   -> return ()
    return $ NTString s

-- Quote makes the context explicitly a quoting one
normTerm_ _   ss (P.TFunctor "&" [t T.:~ st]) = do
    normTerm_ (ECExplicit,ADQuote) (st:ss) t

-- Evaluation is a little different: in addition to forcing the context to
-- evaluate, it must also evaluate if the context from on high is one of
-- evaluation!
normTerm_ c   ss (P.TFunctor "*" [t T.:~ st]) =
    normTerm_ (ECExplicit,ADEval) (st:ss) t
    >>= \nt -> case c of
                (_,ADEval) -> case nt of
                                NTVar v -> NTVar `fmap` newEval "_s" (Left v)
                                _       -> do
                                            newWarn "Ignoring * of literal" ss
                                            return nt
                _          -> return nt

-- "is/2" is sort of exciting.  We normalize the second argument in an
-- evaluation context and the first in a quoted context.  Then, if the
-- result is quoted, we simply build up some structure.  If it's evaluated,
-- on the other hand, we reduce it to a unification of these two pieces and
-- return (XXX what ought to be) a unit.
normTerm_ c   ss (P.TFunctor "is" [x T.:~ sx, v T.:~ sv]) = do
    nx <- normTerm_ (ECFunctor, ADQuote) (sx:ss) x >>= newAssign "_i" . Left
    nv <- normTerm_ (ECFunctor, ADEval ) (sv:ss) v >>= newAssign "_i" . Left
    case c of
        (_,ADEval) -> do
                       _ <- doUnif nx nv
                       return $ NTNumeric (Left 42)  -- XXX ought to be NTUnit
        _          -> do
                       NTVar `fmap` newAssign "_u" (Right ("is",[nx,nv]))

-- Annotations 
--
-- XXX this is probably the wrong thing to do
normTerm_ c   ss (P.TAnnot a (t T.:~ st)) = do
    v <- normTerm_ c (st:ss) t >>= newAssignNT "_a"
    newAnnot v a
    return (NTVar v)

-- Functors have both top-down and bottom-up dispositions on
-- their handling.
normTerm_ c   ss (P.TFunctor f as) = do

    argdispos <- asks $ flip ($) (f,length as) . ad_arg_dispos
    normas <- mapM (\(a T.:~ s,d) -> normTerm_ (ECFunctor,d) (s:ss) a)
                   (zip as argdispos)

    -- Convert everything to DVars and, while here, do a linearization
    -- pass to strip duplicate vars out.  We need pattern matching to be
    -- linear-with-checks in later pipeline stages so that we can, for
    -- example, correctly reject updates that are not the right shape.
    normas' <- let delin (vs,r) x = do
                     case x of
                       x@(NTVar v) | v `elem` vs -> do
                            v' <- newAssign   "_x" (Left x)
                            return (vs,v':r)
                       NTVar v -> do
                            return (v:vs,v:r)
                       _ -> do
                            v' <- newAssignNT "_x" x
                            return (vs,v':r)
               in (reverse . snd) `fmap` foldM delin ([],[]) normas

    selfdispos <- asks $ flip ($) (f,length as) . ad_self_dispos

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

data FRule = FRule { fr_functor :: DVar
                   , fr_aggregator :: DAgg
                   , fr_side :: [DVar]
                   , fr_result :: DVar
                   , fr_span :: T.Span
                   , fr_anf :: ANFState }
 deriving (Show)

-- XXX
normRule :: T.Spanned P.Rule   -- ^ Term to digest
         -> FRule
normRule (P.Rule h a es r T.:~ span) = uncurry ($) $ runNormalize $ do
    nh  <- normTerm False h >>= newAssignNT "_h"
    nr  <- normTerm True  r >>= newAssignNT "_r"
    nes <- mapM (\e -> normTerm True e >>= newAssignNT "_c") es
    return $ FRule nh a nes nr span

------------------------------------------------------------------------}}}
-- Run the normalizer                                                   {{{

-- | Run the normalization routine.
--
-- Use as @runNormalize nRule
runNormalize :: ReaderT ANFDict (State ANFState) a -> (a, ANFState)
runNormalize =
  flip runState   (AS 0 M.empty M.empty [] M.empty []) .
  flip runReaderT (AD dynaFunctorArgDispositions dynaFunctorSelfDispositions)

------------------------------------------------------------------------}}}
-- Pretty Printer                                                       {{{

printANF :: FRule -> Doc e
printANF (FRule h a s result span
            (AS {as_evals = evals, as_assgn = assgn, as_unifs = unifs})) =
          text ";;" <+> prettySpanLoc span
  `above`
  ( parens $ (pretty a)
            <+> valign [ (pretty h)
                       , parens $ text "side"   <+> (valign $ map pretty s)
                       , parens $ text "evals"  <+> pev
                       , parens $ text "unifs"  <+> pun
                       , parens $ text "result" <+> (pretty result)
                       ]
  ) <> line
  where
    pft :: FDT -> Doc e
    pft (fn,args)  = parens $ hsep $ (pretty fn : (map pretty args))

    pevf :: EVF -> Doc e
    pevf (Left v)   = pretty v
    pevf (Right t)  = pft t

    penf :: ENF -> Doc e
    penf (Left n)   = pretty n
    penf (Right t)  = pft t

    pev = valign $ map (\(y,z)-> parens $ pretty y <+> pevf z)
                 $ M.toList evals

    pun = valign $    map (\(y,z)-> parens $ pretty y <+> penf z)
                          (M.toList assgn)
                   ++ map (\(y,z) -> parens $ pretty y <+> pretty z)
                          unifs

------------------------------------------------------------------------}}}
