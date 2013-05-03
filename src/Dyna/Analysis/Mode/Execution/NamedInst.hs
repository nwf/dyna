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
{-# OPTIONS_GHC -Wall #-}

module Dyna.Analysis.Mode.Execution.NamedInst (
    -- * Datatype definition
    NIX(..), NIXM,
    -- * Well-formedness predicates
    nWellFormedUniq, nWellFormedOC,
    -- * Unary functions
    nNotEmpty, nGround, nUpUniq, nPrune, nExpose, nHide,
    -- * Binary comparators
    nCmp, nEq, nLeq, nSub,
    -- * Total binary functions
    nTBin, nLeqGLB, nSubGLB,
    -- * Partial binary functions
    nPBin, nLeqGLBRD, nLeqGLBRL, -- nSubLUB,
    -- * Mode functions
    mWellFormed,
) where

import           Control.Applicative
import qualified Control.Arrow                     as A
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.Foldable                     as F
import qualified Data.HashSet                      as H
import qualified Data.Map                          as M
import qualified Data.Set                          as S
import qualified Data.Traversable                  as T
-- import qualified Debug.Trace                       as XT
import           Dyna.Analysis.Mode.Inst
import           Dyna.Analysis.Mode.Mode
import           Dyna.Analysis.Mode.Unification
import           Dyna.Analysis.Mode.Uniq
import           Dyna.Main.Exception
import           Dyna.XXX.MonadUtils
import           System.IO.Unsafe (unsafePerformIO)
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
type NIXM a f = M.Map a (Either (NIX f) (InstF f a))

-- | A closed, mu-recursive inst.
--
-- The accessors and constructor is exported solely for the selftests'
-- benefits and SHOULD NOT be used elsewhere in the code!
data NIX f = forall a . (Ord a,Show a) =>
  NIX
  {
    -- | The top InstF ply in this term.
    --
    -- Note that while we could, in theory, have used @:: a@ here,
    -- this would complicate the out-of-phase branch of comparison
    -- operators.  Moreover, it's probably a good idea to use a ply
    -- at the top as it forces NIX to be productive and not just
    -- immediately alias another NIX via its map.
    nix_root  :: InstF f a
  , nix_map   :: NIXM a f
  }

-- XXX This is hideously ugly, but we can clean it up later
instance (Show f) => Show (NIX f) where
 show (NIX r m) = "(NIX ("++ (show r) ++ ") (" ++ (show m) ++ "))"

instance (Pretty f) => Pretty (NIX f) where
 pretty (NIX r m) = ri r <> if M.null m
                             then empty
                             else align $ indent 2
                                  (    (text "where")
                                   <+> (vsep $ map rme $ M.toList m))
  where
   -- render index
   rix = angles . text . show

   -- render map entry
   rme (k,v) = rix k <+> equals <+> either pretty ri v

   -- render uniq (XXX own pretty instance instead?)
   ru u = text $ case u of
                   UUnique          -> "un"
                   UMostlyUnique    -> "mu"
                   UShared          -> "sh"
                   UMostlyClobbered -> "mc"
                   UClobbered       -> "cl"

   -- render inst
   ri IFree          = text "$f"
   ri (IAny u)       = text "$a@" <> ru u
   ri (IUniv u)      = text "$u@" <> ru u
   ri (IBound u bm b) = ru u
                     <> (semiBraces $ if b then (text "$b"):rm else rm)
    where
     rm = map (\(k,vs) -> pretty k <> tupled (map rix vs)) (M.toList bm)
                    

------------------------------------------------------------------------}}}
-- Utilities                                                            {{{

-- | Throw exception if ever a NIX is not well formed
--
-- XXX Should attempt to do something with the 'NIX' passed in!
panicwf :: NIX f -> a
panicwf _ = dynacPanicStr "NIX not well formed"

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
eml :: (Monad m, Ord k)
    => NIX f -- ^ For debugging
    -> (a -> m c)
    -> (b -> m c)
    -> M.Map k (Either a b)
    -> k
    -> m c
eml n al ar m x = either al ar (ml n m x)

-- | Our particular version of 'fromJust' which panics appropriately.
ml :: (Ord k) => NIX f -> M.Map k b -> k -> b
ml n m x = maybe (panicwf n) id (M.lookup x m)

------------------------------------------------------------------------}}}
-- Unary predicates                                                     {{{

-- | Check well-formedness of an inst at a given Uniq.  All uniqueness
-- annotations within the inst are required to be larger (i.e. less unique,
-- more restrictive).
--
-- Additionally, each position in the inst may have only one Uniq value
-- on any path that reaches it.
nWellFormedUniq :: forall f . (Show f) => Uniq -> NIX f -> Bool
nWellFormedUniq u0 n0@(NIX i0 m) = evalState (iWellFormed_ q u0 i0)
                                             M.empty
 where
  q u a = -- XT.traceShow ("NWFU Q",u,a,ml n0 m a) $
          do
           cached <- gets (M.lookup a)
           case cached of
             Nothing -> do
                         id %= M.insert a u
                         eml n0 (return . nWellFormedUniq u)
                                (iWellFormed_ q u)
                                m a
             Just u' -> return $ u == u'

-- | Check that a named inst is acyclic.
--
-- Makes use of the 'StableName' functionality (and 'unsafePerformIO') to
-- ensure that the Haskell heap is acyclic.  This is likely useful for
-- debugging nontermination of the compiler.  There's nothing that can save
-- us from an evil NIX which generates additional NIXes on the fly, tho'.
--
nWellFormedOC :: forall f . (Ord f) => NIX f -> ()
nWellFormedOC = go H.empty
 where
  mksp x = x `seq` unsafePerformIO (makeStableName x)

  visit q i = F.mapM_ (F.mapM_ q) (i ^. inst_rec)

  go vis n0@(NIX i m) = 
    let sn0 = mksp n0
    in if sn0 `H.member` vis
        then dynacPanicStr "Named inst occurs check!"
        else evalState (visit (q vis) i) S.empty
   where
    q v a = tsc id a
            (eml n0 (return . go v) (visit (q v)) m a >> return True)

-- | Is a named inst ground?
nGround :: forall f . NIX f -> Bool
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
nNotEmpty :: forall f . NIX f -> Bool
nNotEmpty n0@(NIX i0 m0) = evalState (visit i0) S.empty
 where
  visit IFree     = return True
  visit (IUniv _) = return True
  visit (IAny _)  = return True
  visit (IBound _ m b) = b `orM1` (anyM $ M.foldr (\fas a -> allM (map rec fas) : a) [] m)

  rec idx = do
    already <- gets (S.member idx)
    already `orM1` eml n0 (return . nNotEmpty)
                          (\v -> modify (S.insert idx) >> visit v) m0 idx

nCrawl :: forall f . (Ord f)
       => (forall a . Uniq -> InstF f a) -- ^ Replace free variables
       -> Uniq                           -- ^ Minimum uniqueness
       -> NIX f
       -> NIX f
nCrawl fv u0 n0@(NIX i0 m) =
  let i0' = reall u0 i0 in NIX i0' $ execState (T.traverse (evac u0) i0') M.empty
 where
  reun = over inst_uniq (max u0)

  refv u IFree = fv u
  refv _ x     = x

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
nPrune :: forall f . (Ord f) => NIX f -> NIX f
nPrune = nCrawl (const IFree) UUnique

-- | Increase the nonuniqueness of a particular named inst to at least the
-- given level.
nUpUniq :: forall f . (Ord f) => Uniq -> NIX f -> NIX f

-- XXX a simplistic, cruft-removing, but inefficient solution.
-- nUpUniq = nCrawl (const IFree)

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
nExpose :: NIX f -> InstF f (NIX f)
nExpose n@(NIX r m) = fmap (\a -> either id (\i -> NIX i m) (ml n m a)) r
{-# INLINABLE nExpose #-}

-- | An inefficient \"inverse\" of nExpose.
nHide :: InstF f (NIX f) -> NIX f
nHide i = uncurry NIX $ runState (T.mapM next i) M.empty
 where
  next n = do
    m <- get
    let n' = M.size m
    put (M.insert n' (Left n) m)
    return n'
{-# INLINABLE nHide #-}

------------------------------------------------------------------------}}}
-- Binary predicates                                                    {{{

nCmp :: forall f . (Ord f, Show f)
     => (forall a b m .
            (Monad m)
         => (a -> InstF f b -> m Bool)
         -> (a -> b -> m Bool)
         -> InstF f a -> InstF f b -> m Bool)
     -> NIX f -> NIX f -> Bool
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

nEq, nLeq, nSub :: (Ord f, Show f) => NIX f -> NIX f -> Bool
nEq  = nCmp (\_ -> iEq_)
nLeq = nCmp iLeq_
nSub = nCmp iSub_

------------------------------------------------------------------------}}}
-- Binary functions                                                     {{{

data NBinState a b f = NBS { _nbs_next  :: Int
                           , _nbs_ctx   :: NIXM Int f
                           , _nbs_cache_symm :: M.Map (Uniq,a,b) Int
                           , _nbs_cache_lsml :: M.Map (Uniq,InstF f b,a) Int
                           , _nbs_cache_lsmr :: M.Map (Uniq,InstF f a,b) Int
                           }
$(makeLenses ''NBinState)

iNBS :: NBinState a b f
iNBS = NBS 0 M.empty M.empty M.empty M.empty


nTBin :: forall f . (Ord f, Show f)
      => (  forall a b c m .
            (Monad m)
         => (Uniq -> a -> m c)
         -> (Uniq -> b -> m c)
         -> (Uniq -> InstF f b -> a -> m c)
         -> (Uniq -> InstF f a -> b -> m c)
         -> (Uniq -> a -> b -> m c)
         -> Uniq
         -> InstF f a -> InstF f b -> m (InstF f c))
      -> NIX f -> NIX f -> NIX f
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
nLeqGLB, nSubGLB :: forall f . (Ord f, Show f) => NIX f -> NIX f -> NIX f
nLeqGLB = nTBin iLeqGLB_
nSubGLB = nTBin (\_ _ fl fr fm _ -> iSubGLB_ (fl UUnique) (fr UUnique) (fm UUnique))

nPBin :: forall e f .
         (Ord f, Show f)
      => (  forall a b c m .
            (Monad m, Show a, Show b, Show c)
         => (Uniq -> a -> m c)
         -> (Uniq -> b -> m c)
         -> (Uniq -> InstF f b -> a -> m c)
         -> (Uniq -> InstF f a -> b -> m c)
         -> (Uniq -> a -> b -> m c)
         -> Uniq
         -> InstF f a -> InstF f b -> m (Either e (InstF f c)))
      -> NIX f -> NIX f -> Either e (NIX f)
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
nLeqGLBRD, nLeqGLBRL :: forall f . (Ord f, Show f) => NIX f -> NIX f -> Either UnifFail (NIX f)
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
mWellFormed :: forall f . (Ord f, Show f) => QMode (NIX f) -> Bool
mWellFormed (QMode ats vm@(vti,vto) _) =
  (all (nWellFormedUniq UUnique)
       $ vti:vto:concatMap (\(i,o) -> [i,o]) ats)
  &&
  (all (uncurry (flip nLeq)) $ vm:ats)

------------------------------------------------------------------------}}}
