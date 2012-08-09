{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Dyna.NormalizeParse where

-- import           Control.Arrow
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.UTF8       as BU
import qualified Data.ByteString            as B
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Text.Trifecta              as T

import qualified Dyna.ParserHS.Parser       as P
import           Dyna.Test.Trifecta         -- XXX

data Term = TFunctor !B.ByteString ![Term]
          | TVar !B.ByteString
 deriving (Eq,Ord,Show)
    -- The Ord instance is only there for the use of M.Map and S.Set
    -- and should not be relied upon to be meaningful in any sense.

data Rule = Rule !Term !B.ByteString ![Term] !Term
 deriving (Eq,Ord,Show)

-- | A map from (functor,arity) to a list of bits indicating whether to
-- (True) or not to (False) evaluate that positional argument.
--
-- XXX This isn't going to work when we get more complicated terms.
--
-- XXX Stronger type desired: we'd like static assurance that the
-- length of the list matches the arity in the key!
--
-- XXX What do we do about certain atoms that don't evaluate?  I don't
-- think I have handled it correctly here.  Maybe we should revisit the
-- "primitives don't evaluate" thing, but that doesn't help for things
-- like "pair/2" which we (presumably) want to be (preferentially)
-- structure-building.
--
data ANFDict = AD
             { ad_arg_dispos  :: M.Map (B.ByteString,Int) [Bool]
             , ad_self_dispos :: S.Set (B.ByteString,Int)
             }


{- This stage of ANF does not actually link evaluations to
 - their semantic interpretation.  That is, we have not yet
 - resolved foreign function calls.
 -}
data ANFState = AS
              { as_next  :: Int
              , as_evals :: S.Set (B.ByteString,Term)
              }
 deriving (Show)

-- data Rule = Rule Term String Term (S.Set (String,Term))
--  deriving (Show)

newEval :: (MonadState ANFState m) => String -> Term -> m Term
newEval pfx t = do
    evs <- gets as_evals
    vn  <- gets as_next
    let n = BU.fromString $ pfx ++ show vn
    put $ AS (vn + 1) (S.insert (n,t) evs)
    return $ TVar n

    -- | Convert a syntactic term into ANF.
normalizeTerm_ :: (MonadState ANFState m, MonadReader ANFDict m)
               => Bool          -- ^ In an evaluation context?
               -> [T.Span]      -- ^ List of spans traversed
               -> P.Term        -- ^ Term being digested
               -> m Term

    -- Variables don't evaluate and don't need to be moved
normalizeTerm_ _     _  (P.TVar  v)       = return $ TVar v

    
    -- Quote simply disappears having converted the context to
    -- a non-evaluation context.
normalizeTerm_ _     ss (P.TFunctor "&" [t T.:~ st]) = do
    normalizeTerm_ False (st:ss) t

    -- Star forces evaluation even when the argument would prefer
    -- to not be evaluated, thus the sort of odd "normalize in
    -- nonevaluation context then eval" here.
normalizeTerm_ _     ss (P.TFunctor "*" [t T.:~ st]) = do
    normalizeTerm_ False (st:ss) t >>= newEval "_normTS_"

    -- Functors have both top-down and bottom-up dispositions on
    -- their handling.
normalizeTerm_ c     ss (P.TFunctor f as) = do
    argdispos <- asks $ maybe (repeat True) (id) . M.lookup (f,length as) . ad_arg_dispos
    normas <- mapM (\(a T.:~ s,d) -> normalizeTerm_ d (s:ss) a) (zip as argdispos)
    selfdispos <- getSelfDispos
    (if c && selfdispos then newEval "_normTF_" else return) $ TFunctor f normas
 where
   getSelfDispos = do
    set <- asks $ not . S.member (f,length as) . ad_self_dispos
    -- XXX numerics
    return set
    

normalizeTerm :: (MonadState ANFState m, MonadReader ANFDict m)
              => Bool               -- ^ In an evaluation context?
              -> T.Spanned P.Term   -- ^ Term to digest
              -> m Term
normalizeTerm c (t T.:~ s) = normalizeTerm_ c [s] t

    -- XXX
dynaFunctorArgDispositions :: M.Map (B.ByteString,Int) [Bool]
dynaFunctorArgDispositions = M.fromList [
    (("is",2),[False,True])
 ]

    -- XXX
    --
    -- Functors which prefer not to be evaluated
dynaFunctorSelfDispositions :: S.Set (B.ByteString,Int)
dynaFunctorSelfDispositions = S.fromList
    [ ("true",0)
    , ("false",0)
    , ("pair",2)
    ]

    -- XXX
run = flip runStateT  (AS 0 S.empty) .
      flip runReaderT (AD dynaFunctorArgDispositions dynaFunctorSelfDispositions)

    -- XXX
testNormTerm = run . normalizeTerm False . unsafeParse P.dterm

normalizeRule (P.Fact t T.:~ _) = do
    nt <- normalizeTerm False t
    return $ Rule nt ":-" [] (TFunctor "true" [])
normalizeRule (P.Rule h a es r T.:~ _) = do
    nh  <- normalizeTerm False h
    nr  <- normalizeTerm True  r
    nes <- mapM (normalizeTerm True) es
    return $ Rule nh a nes nr

testNormRule = run . normalizeRule . unsafeParse P.drule

{-
neis    e = newEval "_normE_"  $ \v -> EIs v e
neiis i e = newEval "_normEI_" $ \v -> EIndir v i e

normalizeExpr :: Maybe String -> Bool -> P.Expr -> State NormState Term
normalizeExpr Nothing  c (P.ETerm t) = normalizeTerm neis      c t
normalizeExpr (Just i) c (P.ETerm t) = normalizeTerm (neiis i) c t

normalizeExpr mi True (P.ENew  e) = do
    e'  <- normalizeExpr mi True e
    case e' of
        TVar ne' -> newEval "_normEN_" $ \v -> ENew v ne'
        _ -> error "New construct did not reduce to variable (help?)"

normalizeExpr mi c (P.EExpr e1 o e2) = do
    e1' <- normalizeExpr mi True e1
    e2' <- normalizeExpr mi True e2
    (if c then maybe neis neiis mi else return) $ TFunctor o [e1', e2']

normalizeExpr mi True (P.EIndir td ti) = do
    td' <- normalizeTerm eh True td
    case td' of
        TVar ntd' -> normalizeExpr (Just ntd') True ti
        _ -> error "Indirection LHS did not reduce to variable (help?)"
 where eh = maybe neis neiis mi

normalizeExpr _ False (P.ENew _)     = error "New in nonevaluation context (help?)"
normalizeExpr _ False (P.EIndir _ _) = error "Indirection in nonevaluation context (help?)"

testNormExpr c s = let x = P.dynaExpr $ L.alexScanTokens s in
    (x, runState (normalizeExpr Nothing c x) (NS 0 []))


nvis    t = newEval "_normV_"  $ \v -> EIs v t

normalizeEval :: P.Eval -> State NormState Eval
normalizeEval (P.EIs s e) = liftM (EIs s) $ normalizeExpr Nothing True e
normalizeEval (P.EEq t1 t2) = do
    t1' <- normalizeTerm nvis False t1
    t2' <- normalizeTerm nvis False t2
    return $ EEq t1' t2'


nris    t = newEval "_normR_"  $ \v -> EIs v t

normalizeRule_ :: P.Rule -> State NormState Rule
normalizeRule_ (P.Rule h (P.Aggr a) r es) = do
    h' <- normalizeTerm nris False h
    r' <- normalizeExpr Nothing True r
    es' <- mapM normalizeEval es
    return $ Rule h' a r' es'

normalizeRule :: P.Rule -> Rule
normalizeRule r =
    let (Rule h a v es, NS _ es') = runState (normalizeRule_ r) (NS 0 [])
     in Rule h a v (es++es')

testNormRule :: String -> Rule
testNormRule s = let r = P.dynaRule $ L.alexScanTokens s in normalizeRule r
-}
