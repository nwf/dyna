---------------------------------------------------------------------------
-- | Some simple analysis to move to ANF.
--
-- In Dyna's surface syntax, there exists both \"in-place evaluation\" and
-- \"in-place construction\".  How do we deal with this?  Well, it's a
-- little messy:
--
--   1. If a term is wrapped by the unary @*@ functor, it is explicitly
--   moved to an ANF evaluation and replaced by a variable.  So
--   @f(*g(X))@ rewrites as @Y is g(X), f(Y)@.
--
--   2. Each functor gets a chance to specify, for each argument, that it
--   would prefer to evaluate a given position.  If so, and if the position
--   is occupied by a non-variable, non-primitive term (e.g., atom or
--   structure) whose disposition is to evaluate and which is not quoted by
--   the unary @&@ functor, then it is moved to an ANF evaluation.  If f
--   prefers to evaluate only its first position, and @g@ is disposed to
--   evaluation by default, @f(g(X),h(Y))@ rewrites as @Z is g(X),
--   f(Z,h(Y))@.
--
--   3. Otherwise, a non-variable, non-primitive term in an argument will
--   be interpreted as structure-building.
--
-- Note that in rules, the head is by default not evaluated (regardless of
-- the disposition of their outer functor), while the body is interpreted as
-- a term expression (or list of term expressions) to be evaluated.
--
-- XXX This is really quite simplistic and is probably a far cry from where
-- we need to end up.  Especially of note is that we do not yet parse any
-- sort of pragmas for augmenting our disposition list.
--
-- XXX The handling for "is/2" is probably wrong.  Right now it's not
-- special at all, but every Dyna program is defined to include
-- @is(X,Y) :- X = *Y.@.  Is that something we should be normalizing out
-- here or should be waiting for some further unfolding optimization phase?

-- Header material                                                      {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Dyna.Analysis.NormalizeParse where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Unification
import qualified Data.ByteString.UTF8       as BU
import qualified Data.ByteString            as B
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Text.Trifecta              as T

import qualified Dyna.ParserHS.Parser       as P
import           Dyna.Term.TTerm
-- import           Dyna.Test.Trifecta         -- XXX

import qualified Data.Char as C

data ANFDict = AD
  { -- | A map from (functor,arity) to a list of bits indicating whether to
    -- (True) or not to (False) evaluate that positional argument.
    --
    -- XXX This isn't going to work when we get more complicated terms.
    --
    -- XXX Stronger type desired: we'd like static assurance that the
    -- length of the list matches the arity in the key!
    ad_arg_dispos  :: (B.ByteString,Int) -> [Bool]

    -- | The set of functors that prefer not to be evaluated.
  , ad_self_dispos :: S.Set (B.ByteString,Int)
  }


{- This stage of ANF does not actually link evaluations to
 - their semantic interpretation.  That is, we have not yet
 - resolved foreign function calls.
 -}
data ANFState = AS
              { as_next  :: !Int
              , as_evals :: M.Map B.ByteString DTerm
              , as_unifs :: M.Map B.ByteString DTerm
              }
 deriving (Show)

nextVar :: (MonadState ANFState m) => String -> m B.ByteString
nextVar pfx = do
    vn  <- gets as_next
    modify (\s -> s { as_next = vn + 1 })
    return $ BU.fromString $ pfx ++ show vn

newEval :: (MonadState ANFState m) => String -> DTerm -> m DTerm
newEval pfx t = do
    n   <- nextVar pfx
    evs <- gets as_evals
    modify (\s -> s { as_evals = M.insert n t evs})
    return $ UVar n

newUnif :: (MonadState ANFState m) => String -> DTerm -> m DTerm
newUnif pfx t = do
    n   <- nextVar pfx
    uns <- gets as_unifs
    modify (\s -> s { as_unifs = M.insert n t uns})
    return $ UVar n


unspan :: T.Spanned P.Term -> DTerm
unspan (P.TVar v T.:~ _)        = UVar v
unspan (P.TNumeric v T.:~ _)    = UTerm $ TNumeric v
unspan (P.TFunctor a as T.:~ _) = UTerm $ TFunctor a $ map unspan as
unspan (P.TAnnot a t T.:~ _)    = UTerm $ TAnnot (fmap unspan a) (unspan t)


-- | Convert a syntactic term into ANF; while here, move to a
-- Control.Unification term representation.
--
-- XXX This sheds span information entirely, which is probably not what we
-- actually want.  Note that we're careful to keep a stack of contexts
-- around, so we should probably do something clever like attach them to
-- operations we extract?
normTerm_ :: (MonadState ANFState m, MonadReader ANFDict m)
               => Bool          -- ^ In an evaluation context?
               -> Bool          -- ^ Unpack unifications?
               -> [T.Span]      -- ^ List of spans traversed
               -> P.Term        -- ^ Term being digested
               -> m DTerm

    -- Variables don't evaluate and don't need to be moved
normTerm_ _ _   _  (P.TVar  v)       = return $ UVar v

    -- Numerics also get returned in-place.
normTerm_ _ _   _  (P.TNumeric n)    = return $ UTerm $ TNumeric n

    -- FIXME: (nwf) Quote simply disappears having converted the context to a
    -- non-evaluation context.
normTerm_ _ _   ss (P.TFunctor "&" [t T.:~ st]) = do
    normTerm_ False True (st:ss) t

    -- Star forces evaluation even when the argument would prefer
    -- to not be evaluated, thus the sort of odd "normalize in
    -- nonevaluation context then eval" here.
normTerm_ _ _   ss (P.TFunctor "*" [t T.:~ st]) = do
    normTerm_ False True (st:ss) t >>= newEval "_s"

     -- Annotations are stripped of their span information
     --
     -- XXX this is probably the wrong thing to do
normTerm_ c u   ss (P.TAnnot a (t T.:~ st)) = do
    nt <- normTerm_ c u (st:ss) t
    return $ UTerm $ TAnnot (fmap unspan a) nt

    -- Functors have both top-down and bottom-up dispositions on
    -- their handling.
normTerm_ c u   ss (P.TFunctor f as) = do
    argdispos <- asks $ flip ($) (f,length as) . ad_arg_dispos
    normas <- mapM (\(a T.:~ s,d) -> normTerm_ d True (s:ss) a) (zip as argdispos)
    selfdispos <- getSelfDispos
    (case () of
       _ | c && selfdispos -> newEval "_f"
       _ | u               -> newUnif "_u"
       _                   -> return)
     $ UTerm $ TFunctor f normas
 where
   getSelfDispos = do
    set <- asks $ not . S.member (f,length as) . ad_self_dispos
    return set


normTerm :: (MonadState ANFState m, MonadReader ANFDict m)
         => Bool               -- ^ In an evaluation context?
         -> T.Spanned P.Term   -- ^ Term to digest
         -> m DTerm
normTerm c (t T.:~ s) = normTerm_ c False [s] t

    -- XXX
normRule :: (MonadState ANFState m, MonadReader ANFDict m)
         => T.Spanned P.Rule   -- ^ Term to digest
         -> m DRule
normRule (P.Fact t T.:~ _) = do
    nt <- normTerm False t
    return $ Rule nt ":-" [] (UTerm $ TFunctor "true" [])
normRule (P.Rule h a es r T.:~ _) = do
    nh  <- normTerm False h
    nr  <- normTerm True  r
    nes <- mapM (normTerm True) es
    return $ Rule nh a nes nr

    -- XXX
dynaFunctorArgDispositions :: (B.ByteString, Int) -> [Bool]
dynaFunctorArgDispositions x = case x of
    ("is", 2) -> [False,True]
    -- evaluate arithmetic / math
    ("exp", 1) -> [True]
    ("log", 1) -> [True]
    -- logic
    ("and", 2) -> [True, True]
    ("or", 2)  -> [True, True]
    ("not", 1) -> [True]
    (name, arity) -> take arity $ repeat $ not.C.isAlphaNum $ head $ BU.toString name

    -- XXX
    --
    -- Functors which prefer not to be evaluated
dynaFunctorSelfDispositions :: S.Set (B.ByteString,Int)
dynaFunctorSelfDispositions = S.fromList
    [ ("true",0)
    , ("false",0)
    , ("pair",2)
    ]


-- | Run the normalization routine.
--
-- Use as @runNormalize nRule
runNormalize :: ReaderT ANFDict (State ANFState) a -> (a, ANFState)
runNormalize =
  flip runState   (AS 0 M.empty M.empty) .
  flip runReaderT (AD dynaFunctorArgDispositions dynaFunctorSelfDispositions)
