---------------------------------------------------------------------------
-- | Self-contained, mu-recursive inst automata
--
-- XXX This could stand a good bit of refactoring out to being generic, but
-- I am writing it quickly in hopes of checking that it works before
-- investing too much more time.

-- Header material                                                      {{{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.Mode.Execution.NamedInst (
    -- * Datatype definition
    NIX(..), NIXM,
    -- * Unary functions
    -- ** Well-formedness predicates
    nWellFormedUniq, nWellFormedOC,
    -- ** Inquiries
    nAllNotEmpty, nSomeNotEmpty, nGround, nUpUniq,
    -- ** Construction
    nHide, nShallow, nDeep,
    -- ** Destruction
    nExpose, 
    -- ** Internals
    nPrune,
    -- * Binary comparators
    nCmp, nEq, nLeq, nSub,
    -- * Total binary functions
    nTBin, nLeqGLB, nSubGLB,
    -- * Partial binary functions
    nPBin, nLeqGLBRD, nLeqGLBRL, -- nSubLUB,
    -- * Mode functions
    mWellFormed,

    -- * XXX
    eml, 
) where

import           Control.Applicative
import qualified Control.Arrow                     as A
import           Control.Lens
-- import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.Foldable                     as F
import qualified Data.HashSet                      as H
import qualified Data.Map                          as M
import qualified Data.Set                          as S
import qualified Data.Traversable                  as T
-- import qualified Debug.Trace                       as XT
import           Dyna.Analysis.Mode.Inst
import qualified Dyna.Analysis.Mode.InstPretty     as IP
import           Dyna.Analysis.Mode.Mode
import           Dyna.Analysis.Mode.Unification
import           Dyna.Analysis.Mode.Uniq
import           Dyna.Main.Exception
-- import           Dyna.XXX.DataUtils
import           Dyna.XXX.MonadUtils
-- import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.StableName (makeStableName)
import           Text.PrettyPrint.Free

------------------------------------------------------------------------}}}
-- Datatype definition                                                  {{{

-- | Each named position in a NIX automata either references another
-- closed term or an InstF ply which itself recurses as labels in this
-- automata.
--
-- Note that we are contractually obligated to keep NIX automata mutually
-- acyclic (i.e. all cycles must be confied within some NIX automata).
type NIXM a fr f = M.Map a (Either (NIX fr f) (InstF fr f a))

-- | A closed, mu-recursive inst.
--
-- The existential captures a @Show f@ as well for use by 'panicwf' below.
-- This requires that all terms constructing NIXes afresh (e.g. 'nHide' and
-- 'nShallow') have a @Show f@ constraint, but means that we don't need to
-- annotate the whole universe.  The trade-off, of course, is that we
-- potentially carry those dictionaries around at runtime.
--
-- The accessors and constructor is exported solely for the selftests'
-- benefits and SHOULD NOT be used elsewhere in the code!
data NIX fr f = forall a . (Ord a,Show a,Show fr,Show f) =>
  NIX
  {
    -- | The top InstF ply in this term.
    --
    -- Note that while we could, in theory, have used @:: a@ here,
    -- this would complicate the out-of-phase branch of comparison
    -- operators.  Moreover, it's probably a good idea to use a ply
    -- at the top as it forces NIX to be productive and not just
    -- immediately alias another NIX via its map.
    nix_root  :: InstF fr f a
  , nix_map   :: NIXM a fr f
  }

-- | Semantic, not structural, equality
instance (Ord fr, Ord f) => Eq (NIX fr f) where
 n1 == n2 = nEq n1 n2

-- XXX This is hideously ugly, but we can clean it up later
instance (Show fr, Show f) => Show (NIX fr f) where
 show (NIX r m) = "(NIX ("++ (show r) ++ ") (" ++ (show m) ++ "))"

------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

-- | Throw exception if ever a NIX is not well formed
panicwf :: NIX fr f -> a
panicwf n = dynacPanic (text "NIX not well-formed"
                        `above` indent 2 (pretty n))

-- | Often we want to check a set cache for membership, returning true if
-- so, or assume this case and run some action to obtain a boolean.  This is
-- used, for example, in cycle-breaking in backward chaining: we assume the
-- provability of our assumption and continue to look for a
-- counter-argument.  Note that this is rather the opposite of most circular
-- systems!
tsc :: (Ord e, MonadState s m)
    => Simple Lens s (S.Set e)
    -> e
    -> m Bool
    -> m Bool
tsc l e miss = (uses l $ S.member e)
               `orM`
               (l %= S.insert e >> miss)

-- | Either of Maybe of Lookup.  A common pattern found in implementation.
eml :: (Ord k)
    => NIX fr f -- ^ For debugging
    -> (a -> c)
    -> (b -> c)
    -> M.Map k (Either a b)
    -> k
    -> c
eml n al ar m x = either al ar (ml n m x)

-- | Our particular version of 'fromJust' which panics appropriately.
ml :: (Ord k) => NIX fr f -> M.Map k b -> k -> b
ml n m x = maybe (panicwf n) id (M.lookup x m)

------------------------------------------------------------------------}}}
-- Unary predicates                                                     {{{

-- | Check well-formedness of an inst at a given Uniq.  All uniqueness
-- annotations within the inst are required to be larger (i.e. less unique,
-- more restrictive).
nWellFormedUniq :: forall fr f . Uniq -> NIX fr f -> Bool
nWellFormedUniq u0 n0@(NIX i0 m) = evalState (iWellFormed_ q u0 i0)
                                             M.empty
 where
  q u a = -- XT.traceShow ("NWFU Q",u,a,ml n0 m a) $
          do
           cached <- gets (M.lookup a)
           case cached of
             Nothing -> rec
                -- If we've been here before, it's OK if we are coming in
                -- more uniquely.  If we are coming in less uniquely (i.e.
                -- with a greater Uniq), then we need to recurse through
                -- this binding again.
             Just u' -> orM1 (u <= u') rec
   where
    rec = do
           id %= M.insert a u
           eml n0 (return . nWellFormedUniq u)
                  (iWellFormed_ q u)
                  m a

-- | Check that a named inst is acyclic.
--
-- Makes use of the 'StableName' functionality (and 'unsafePerformIO') to
-- ensure that the Haskell heap is acyclic.  This is likely useful for
-- debugging nontermination of the compiler.  There's nothing that can save
-- us from an evil NIX which generates additional NIXes on the fly, tho'.
--
nWellFormedOC :: (Ord f) => NIX fr f -> IO ()
nWellFormedOC n0 = evalStateT (go n0) H.empty
 where
  mksp x = x `seq` makeStableName x

  visit q i = F.mapM_ (F.mapM_ q) (i ^. inst_rec)

  go n@(NIX i m) = do
    sn <- liftIO $ mksp n
    vis <- get
    if sn `H.member` vis
     then dynacPanicStr "Named inst occurs check!"
     else do
           put (H.insert sn vis)
           evalStateT (visit q i) S.empty
   where
    q a = tsc id a
            (eml n0 (lift . go) (visit q) m a >> return True)

-- | Is a named inst ground?
nGround :: forall fr f . NIX fr f -> Bool
nGround n0@(NIX i0 m) = evalState (iGround_ q i0) S.empty
 where
  q a = tsc id a $ eml n0 (return . nGround) (iGround_ q) m a
  {-
  q (Left a)  = return $ nGround a 
  q (Right a) = tsc id a $ ml n0 m a >>= iGround_ q
  -}

-- | Is there some term not ruled out by this inst?
--
-- This is mostly useful for the test harness, not actual reasoning, at
-- the moment, since we are not sufficiently precise (i.e. we will miss some
-- empty unification results).
nSomeNotEmpty :: forall fr f . NIX fr f -> Bool
nSomeNotEmpty = fix (nNotEmpty_core orAny)
 where orAny b bs = b `orM1` (anyM bs)

-- | Like 'nNotEmpty' but conjunctive across choices -- that is, this
-- requires that all possible branches of an automata are non-empty, rather
-- than 'nNotEmpty', which only checks that there is some reachable state in
-- the automata.
nAllNotEmpty :: forall fr f . (Show f) => NIX fr f -> Bool
nAllNotEmpty = fix (nNotEmpty_core andAll)
 where andAll b bs = b `andM1` (allM bs)



nNotEmpty_core :: forall fr f .
                  (forall m .
                     (Monad m)
                  => Bool
                  -> [m Bool]
                  -> m Bool)
               -> (NIX fr f -> Bool)
               -> NIX fr f -> Bool
nNotEmpty_core disj self n0@(NIX i0 m0) = evalState (visit i0) S.empty
 where
  visit (IFree _) = return True
  visit (IUniv _) = return True
  visit (IAny _)  = return True
  visit (IBound _ m b) = b `disj` (M.foldr (\fas a -> allM (map rec fas) : a) [] m)

  rec idx = tsc id idx (eml n0 (return . self) visit m0 idx)

-- | Increase the nonuniqueness of a particular named inst to at least the
-- given level.
--
-- This would be equivalent to unification with 'IANy' at the given 'Uniq'
-- level, save that it leaves free variables untouched.
nUpUniq :: forall fr f . (Ord f) => Uniq -> NIX fr f -> NIX fr f
{-
 - XXX The beginnings of a possibly more efficient implementation
nUpUniq u0 n0@(NIX i0 m) = uncurry NIX $ runState (T.traverse visit i0) m
 where
  visit a = eml n0 (return . nUpUniq u0)
                   (T.traverse visit (over inst_uniq (max u0)))
-}
nUpUniq u0 n0@(NIX i0 m0) =
   maybe n0 (\u' -> if u' >= u0 then n0 else NIX i0' m0') (iUniq i0)
 where
  reuniq = over inst_uniq (max u0)

  m0' = M.map (nUpUniq u0 A.+++ reuniq) m0
  i0' = reuniq i0
{-# INLINABLE nUpUniq #-}

-- | Expose the root ply of a 'NIX' as an Inst which recurses as additional
-- 'NIX' elements.
--
-- Note that recursive use of this function may well diverge!
nExpose :: NIX fr f -> InstF fr f (NIX fr f)
nExpose n@(NIX r m) = fmap (\a -> either id (\i -> NIX i m) (ml n m a)) r
{-# INLINABLE nExpose #-}

-- | An inefficient \"inverse\" (up to isomorphism) of nExpose.
nHide :: (Show fr, Show f) => InstF fr f (NIX fr f) -> NIX fr f
nHide i = uncurry NIX $ runState (T.mapM next i) M.empty
 where
  next n = do
    m <- get
    let n' = M.size m
    put (M.insert n' (Left n) m)
    return n'
{-# INLINABLE nHide #-}

nShallow :: (Show fr, Show f) => InstF fr f a -> Maybe (NIX fr f)
nShallow (IFree f)      = Just $ nHide $ IFree f
nShallow (IAny u)       = Just $ nHide $ (IAny u)
nShallow (IUniv u)      = Just $ nHide $ (IUniv u)
nShallow (IBound _ _ _) = Nothing
{-# INLINABLE nShallow #-}

nDeep :: (Show fr, Show f, Monad m, Functor m)
      => (r -> m (Either (NIX fr f) (InstF fr f r)))
      -> InstF fr f r 
      -> m (NIX fr f)
nDeep rec root = liftM (\(nr,(_,nm)) -> NIX nr nm) $
  flip runStateT (0 :: Int, M.empty) $ inst_recps rec' root
 where
  rec' r = do
    a  <- _1 <<%= (+(1 :: Int))
    rhs <- lift (rec r)
    rhs' <- either (return . Left) (liftM Right . inst_recps rec') rhs
    _2 %= M.insert a rhs'
    return a



------------------------------------------------------------------------}}}
-- Binary predicates                                                    {{{

nCmp :: forall fr f .
        (Ord fr, Ord f)
     => (forall a b m .
            (Monad m)
         => (a -> InstF fr f b -> m Bool)
         -> (a -> b -> m Bool)
         -> InstF fr f a -> InstF fr f b -> m Bool)
     -> NIX fr f -> NIX fr f -> Bool
nCmp q l0@(NIX li0 lm) r0@(NIX ri0 rm) =
  evalState (q qop qip li0 ri0) (S.empty, S.empty)
 where
  -- Q In Phase
  qip l r  = -- XT.traceShow ("NCMP QIP",l,r) $
             tsc _1 (l,r) $ do
               eli <- maybe (panicwf l0) return $ M.lookup l lm
               eri <- maybe (panicwf r0) return $ M.lookup r rm
               case (eli,eri) of
                 (Left  l', Left  r') -> return $ nCmp q l' r'
                 (Left  l', Right r') -> return $ nCmp q l' (NIX r' rm)
                 (Right l', Left  r') -> return $ nCmp q (NIX l' lm) r'
                 (Right l', Right r') -> q qop qip l' r'

  -- Q Out of Phase
  qop l ri = -- XT.traceShow ("NCMP QOP",l,ri) $
             tsc _2 (l,ri) $ eml l0 (return . flip (nCmp q) (NIX ri rm))
                                    (flip (q qop qip) ri)
                                    lm l

nEq, nLeq, nSub :: (Ord fr, Ord f) => NIX fr f -> NIX fr f -> Bool
nEq  = nCmp (\_ -> iEq_)
nLeq = nCmp iLeq_
nSub = nCmp iSub_

------------------------------------------------------------------------}}}
-- Binary functions                                                     {{{

data NBinState a b fr f u = NBS
                          { _nbs_next  :: Int
                          , _nbs_ctx   :: NIXM Int fr f
                          , _nbs_cache_symm :: M.Map (u,a,b) Int
                          , _nbs_cache_lsml :: M.Map (u,InstF fr f b,a) Int
                          , _nbs_cache_lsmr :: M.Map (u,InstF fr f a,b) Int
                          }
$(makeLenses ''NBinState)

iNBS :: NBinState a b fr f u
iNBS = NBS 0 M.empty M.empty M.empty M.empty


nTBin :: forall fr f . (Ord fr, Ord f, Show f)
      => (  forall a b c m .
            (Monad m)
         => (Uniq -> a -> m c)
         -> (Uniq -> b -> m c)
         -> (Uniq -> InstF fr f b -> a -> m c)
         -> (Uniq -> InstF fr f a -> b -> m c)
         -> (Uniq -> a -> b -> m c)
         -> Uniq
         -> InstF fr f a -> InstF fr f b -> m (InstF fr f c))
      -> NIX fr f -> NIX fr f -> NIX fr f
nTBin f l0@(NIX li0 lm) r0@(NIX ri0 rm) = evalState (tlq li0 ri0) iNBS
 where
  tlq l r = do
    ci <- f' S.empty UUnique l r
    ctx <- use nbs_ctx
    return $ nPrune $ NIX ci ctx

  f' sc = f (imp l0 lm) (imp r0 rm) (lsml sc) (lsmr sc) (merge sc)

  -- XXX import needs some caching, too.

  -- Occasionally, we need to "import" a term from one of the two inputs;
  -- this happens when we unify 'IBound' against 'IAny' or 'IUniv', for
  -- example.
  --
  -- To import the key x from the map m into our context,
  --   if it is a closed term, just return that
  --   otherwise, make it a closed term whose root is x and whose map is m
  imp n m u x = 
     -- XT.traceShow ("NTB I",u,m,x)
     eml n return (return . flip NIX m) m x >>= new . Left . nUpUniq u 

  new  x = do
    k <- nbs_next <<%= (+1)
    nbs_ctx %= M.insert k x
    return k

  lsml sc u ir l = -- XT.traceShow ("NTB L",u,ir,l) $
                   do
    cached <- uses nbs_cache_lsml $ M.lookup (u,ir,l)
    maybe merge' return cached
   where
    merge' = do
      k <- nbs_next <<%= (+1)
      nbs_cache_lsml %= M.insert (u,ir,l) k
      v <- eml r0
               (return . luu u . nTBin f (NIX ir rm))
               (\l' -> Right . over inst_uniq (max u) <$> f' (S.insert k sc) u l' ir)
               lm l
      nbs_ctx %= M.insert k v
      return k
  
  lsmr sc u il r = -- XT.traceShow ("NTB R",u,il,r) $
                   do
    cached <- uses nbs_cache_lsmr $ M.lookup (u,il,r)
    maybe merge' return cached
   where
    merge' = do
      k <- nbs_next <<%= (+1)
      nbs_cache_lsmr %= M.insert (u,il,r) k
      v <- eml r0
               (return . luu u . nTBin f (NIX il lm))
               (\r' -> Right . over inst_uniq (max u) <$> f' (S.insert k sc) u il r')
               rm r
      nbs_ctx %= M.insert k v
      return k

  luu u = Left . nUpUniq u

  merge sc u l r = -- XT.traceShow ("NTB M",u,l,r) $
                   do
    cached <- uses nbs_cache_symm $ M.lookup (u,l,r)
    maybe merge' return cached
   where
    merge' = do
      k <- nbs_next <<%= (+1)
      nbs_cache_symm %= M.insert (u,l,r) k
      eli <- maybe (panicwf l0) return $ M.lookup l lm
      eri <- maybe (panicwf r0) return $ M.lookup r rm
      v <- case (eli,eri) of
             (Left  l', Left  r') -> return $ luu u $ nTBin f l' r'
             (Left  l', Right r') -> return $ luu u $ nTBin f l' (NIX r' rm)
             (Right l', Left  r') -> return $ luu u $ nTBin f (NIX l' lm) r'
             (Right l', Right r') -> Right . over inst_uniq (max u)
                                     <$> f' (S.insert k sc) u l' r'
      nbs_ctx %= M.insert k v
      return k

-- | Total lattice functions
nLeqGLB, nSubGLB :: forall fr f .
                    (Ord fr, Ord f, Show f)
                 => NIX fr f -> NIX fr f -> NIX fr f
nLeqGLB = nTBin iLeqGLB_
nSubGLB = nTBin (\_ _ fl fr fm _ -> iSubGLB_ (fl UUnique) (fr UUnique) (fm UUnique))

nPBin :: forall e fr f .
         (Ord fr, Ord f, Show f)
      => (  forall a b c m .
            (Monad m, Show a, Show b, Show c)
         => (Uniq -> a -> m c)
         -> (Uniq -> b -> m c)
         -> (Uniq -> InstF fr f b -> a -> m c)
         -> (Uniq -> InstF fr f a -> b -> m c)
         -> (Uniq -> a -> b -> m c)
         -> Uniq
         -> InstF fr f a -> InstF fr f b -> m (Either e (InstF fr f c)))
      -> NIX fr f -> NIX fr f -> Either e (NIX fr f)
nPBin f l0@(NIX li0 lm) r0@(NIX ri0 rm) = evalState (runEitherT (tlq li0 ri0)) iNBS
 where
  tlq l r = do
    ci <- f' S.empty UUnique l r
    ci' <- hoistEither ci
    ctx <- use nbs_ctx
    return $ nPrune $ NIX ci' ctx

  f' sc = f (imp l0 lm) (imp r0 rm) (lsml sc) (lsmr sc) (merge sc)

  luu u = Left . nUpUniq u
  mluu u r = luu u <$> hoistEither r

  -- Occasionally, we need to "import" a term from one of the two inputs;
  -- this happens when we unify 'IBound' against 'IAny' or 'IUniv', for
  -- example.
  --
  -- To import the key x from the map m into our context,
  --   if it is a closed term, just return that
  --   otherwise, make it a closed term whose root is x and whose map is m
  imp n m u x = {- XT.traceShow ("NPB I",m,u,x) $ -}
    new . luu u =<< eml n return (return . flip NIX m) m x

  new  x = do
    k <- nbs_next <<%= (+1)
    nbs_ctx %= M.insert k x
    return k

  lsml sc u ir l = {- XT.traceShow ("NPB L",u,ir,l) $ -}
                   do
    cached <- uses nbs_cache_lsml $ M.lookup (u,ir,l)
    maybe merge' return cached
   where
    merge' = do
      k <- nbs_next <<%= (+1)
      nbs_cache_lsml %= M.insert (u,ir,l) k
      v <- eml r0
               (mluu u . nPBin f (NIX ir rm))
               (\l' -> do
                  l'' <- f' (S.insert k sc) u l' ir
                  (Right . over inst_uniq (max u)) <$> hoistEither l'')
               lm l
      nbs_ctx %= M.insert k v
      return k
  
  lsmr sc u il r = {- XT.traceShow ("NPB R",u,il,r) $ -}
                   do
    cached <- uses nbs_cache_lsmr $ M.lookup (u,il,r)
    maybe merge' return cached
   where
    merge' = do
      k <- nbs_next <<%= (+1)
      nbs_cache_lsmr %= M.insert (u,il,r) k
      v <- eml r0
               (mluu u . nPBin f (NIX il lm))
               (\r' -> do
                  r'' <- f' (S.insert k sc) u il r'
                  (Right . over inst_uniq (max u)) <$> hoistEither r'')
               rm r
      nbs_ctx %= M.insert k v
      return k


  merge sc u l r = {- XT.traceShow ("NPB M",u,l,r) $ -}
                   do
    cached <- uses nbs_cache_symm $ M.lookup (u,l,r)
    maybe merge' return cached
   where
    merge' = do
      k <- nbs_next <<%= (+1)
      nbs_cache_symm %= M.insert (u,l,r) k
      eli <- maybe (panicwf l0) return $ M.lookup l lm
      eri <- maybe (panicwf r0) return $ M.lookup r rm
      v <- case (eli,eri) of
             (Left  l', Left  r') -> mluu u $ nPBin f l' r'
             (Left  l', Right r') -> mluu u $ nPBin f l' (NIX r' rm)
             (Right l', Left  r') -> mluu u $ nPBin f (NIX l' lm) r'
             (Right l', Right r') -> do
                                      m' <- f' (S.insert k sc) u l' r'
                                      (Right . over inst_uniq (max u))
                                         <$> hoistEither m'
      nbs_ctx %= M.insert k v
      return k

-- | Partial lattice functions.  These raise unification failures if
-- the runtime would fail.
nLeqGLBRD, nLeqGLBRL :: forall fr f .
                        (Ord fr, Ord f, Show f)
                     => NIX fr f -> NIX fr f -> Either UnifFail (NIX fr f)
nLeqGLBRD = nPBin iLeqGLBRD_
nLeqGLBRL = nPBin iLeqGLBRL_

{-

XXX BITROTTED; NOT YET -- need better understanding of the problem.  The ⊔
function is particularly interesting and it is not yet clear how to define
its recursion in a way that is not painfully special-cased.  At this instant
I have other things that can demand attention.


nSubLUB :: forall f . (Ord f, Show f) => NIX f -> NIX f -> Maybe (NIX f)
nSubLUB = nPBin (\il ir ll lr m -> iSubLUB_ il ir (ll UClobbered) (lr UClobbered) (m UClobbered))
-}

------------------------------------------------------------------------}}}
-- Mode functions                                                       {{{

-- | Check that all names in a mode are indeed well-formed and that all
-- transitions are according to ≼.
--
-- This lives in Execution.NamedInst because it requires that we be using
-- named insts within the 'QMode'.
--
-- See prose, p35.
mWellFormed :: forall fr f . (Ord fr, Ord f, Show f) => QMode (NIX fr f) -> Bool
mWellFormed (QMode ats vm@(vti,vto) _) =
  (all (nWellFormedUniq UUnique)
       $ vti:vto:concatMap (\(i,o) -> [i,o]) ats)
  &&
  (all (uncurry (flip nLeq)) $ vm:ats)

------------------------------------------------------------------------}}}
-- Cleanup and minimization                                             {{{

nCrawl :: forall fr f .
          (forall a . fr -> Uniq -> InstF fr f a) -- ^ Replace free variables
       -> Uniq                                    -- ^ Minimum uniqueness
       -> NIX fr f
       -> NIX fr f
nCrawl fv u0 n0@(NIX i0 m) =
  let i0' = reall u0 i0 in NIX i0' $ execState (T.traverse (evac u0) i0') M.empty
 where
  reun = over inst_uniq (max u0)

  refv u (IFree f) = fv f u
  refv _ x         = x

  reall u = reun . refv u

  evac u i = gets (M.lookup i) >>= maybe (go u i) (const $ return ())

  go u i = do
    let l = ml n0 m i
    case l of
      Left n  -> id %= M.insert i (Left $ nCrawl fv u n)
      Right x -> do
                  id %= M.insert i (Right $ reall u x)
                  F.traverse_ (evac (maybe u (max u) $ iUniq x)) x
                  return ()

-- | Prune the internals of a 'NIX'.  This really ought not be needed, but
-- it's handy for test generation.
nPrune :: forall fr f . NIX fr f -> NIX fr f
nPrune = nCrawl (\f _ -> IFree f) UUnique

------------------------------------------------------------------------}}}
-- Pretty-printing                                                      {{{

instance Pretty (NIX fr f) where
 pretty (nPrune -> NIX r m) = align $
   ri r <> if M.null m
            then empty
            else line <> (indent 2 $
                            text "where"
                            <+> (align $ vsep $ map rme $ M.toList m))
  where
   -- render index
   rix = angles . text . show

   -- render map entry
   rme (k,v) = rix k <+> equals <+> either pretty ri v

   ri = IP.compactly (text . show) (text . show) rix 



------------------------------------------------------------------------}}}
