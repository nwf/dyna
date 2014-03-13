---------------------------------------------------------------------------
-- | Reimplementation of the Mercury mode system (Inst)
--
-- This module contains the definitions of instantiation states and the
-- primitive predicates on insts.  For clarity and flexibility of
-- implementation, many of these primitive predicates are parameterized on
-- some 'Monad' and defined in terms of open recursion.  This leaves it to
-- the outer 'Monad' to deal with (or not, if termination isn't important ;)
-- ) a lot of technicalities that would obscure the exposition of the
-- thesis' material.
--
-- The thesis has one big datatype for all branches of Inst, with
-- restrictions that some branches may not be used in certain places.
-- Because I think it will be easier to understand in the long run, I have
-- opted to avoid that -- the InstF functor contains only those constructors
-- necessary for building plies of an actual inst.  The other (co)products
-- needed at runtime may be found as parts of the
-- 'Dyna.Analysis.Mode.Execution' modules.

-- Header material                                                      {{{
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Dyna.Analysis.Mode.Inst(
  InstF(..), inst_uniq, inst_rec, inst_recps, inst_irecps,
  iNotReached, iIsNotReached,
  iUniq, iIsFree, iWellFormed_, iEq_, iGround_,
  iLeq_, iLeqGLB_, TyILeqGLB_,
  iSub_, iSubGLB_, iSubLUB_,
) where

import           Control.Applicative (Applicative)
import           Control.Lens
import           Control.Monad
import qualified Data.Foldable            as F
import qualified Data.Traversable         as T
import qualified Data.Map                 as M
import qualified Data.Maybe               as MA
-- import qualified Data.Set                 as S
import           Dyna.Analysis.Mode.Uniq
import qualified Dyna.XXX.DataUtils       as XDU
import           Dyna.XXX.MonadUtils
import qualified Prelude.Extras           as PE

-- import qualified Debug.Trace                       as XT

------------------------------------------------------------------------}}}
-- Instantiation States                                                 {{{

-- | Instantiation states, parametric in Prolog functor @f@ with open
-- recursion through @i@.
--
-- We differ from the mercury thesis and implementation (see prose, p60,
-- \"Obviously, it is not...\") in the use of open recursion rather than a
-- @defined_inst@ branch in the @InstF@.  Rather, we use a disjunctive @i@.
-- Similarly, we use our disjunctive @i@ rather than a separate @alias@
-- constructor (see figure 5.3, p94).
--
-- It's also worth pointing out, while here, that the mode system is
-- concerned only with actual data, not types.  There is clearly some
-- overlap with a type system, and we may wish to investigate adding typing
-- information to this system rather than brew an entirely separate type
-- system (XXX).  In particular, the importance here is that 'IBound' and
-- 'IUniv' should be viewed as intersected with the type information, if
-- any.
--
-- Based on figure 5.3, p94 (but see figure 3.17, p50, for easier cases)
--
-- The 'Ord' instance is solely for internal use; for reasoning, use lattice
-- functions.
data InstF f i =
  -- | An unbound inst.
  --
  -- If you like a machine-core representation-centric view of
  -- the universe, such a thing is a pointer whose own space has
  -- been allocated already but is not pointing to anything yet.
  -- Rules which bind free variables engage in allocation and
  -- fill in these holes.
  --
  -- XXX a boolean flag is introduced in §5.4.1, p103; that
  -- is not yet plumbed through here.
    IFree

  -- | A possibly-bound inst.  We have lost track of whether or
  -- not the given variable is free.  Note that this has runtime
  -- implications: we actively need a bit of data to indicate
  -- whether this value is bound or free, so that we may
  -- dynamically dispatch.  That has implications for any call
  -- whose mode results in the creation of an 'IAny' inst.
  --
  -- Note that we are saying nothing about the possible data
  -- bound by this variable; that would be the job of the type
  -- system. (XXX Maybe we should change that)
  | IAny   { _inst_uniq :: !Uniq }

  -- | A bound inst.  More specifically, a disjunction of
  -- possible binding states: it's guaranteed to be one of these
  -- functors and its associated insts.
  --
  -- The thesis uses sets inside bound, but has the caveat that
  -- a function symbol may occur at most once (defn 3.1.2, p31),
  -- which justifies our use of 'M.Map' here.
  --
  -- Note that defition 3.2.11 (p50) requires that the
  -- uniqueness of the inner Insts be below by <=
  -- (see the 'iWellFormed_' predicate below)
  --
  -- The Bool field, which is an extension from the thesis,
  -- indicates the possibility that this inst is
  -- bound to a base-case of the term universe.
  --
  -- XXX It's possible that we would want a @[TBaseSkolem]@
  -- field for tracking which base-cases, or even a @[TBase]@
  -- field for recording possible actual base-cases, too,
  -- but one thing at a time.
  --
  -- XXX The thesis uses lists, of course, for each functor's arguments.  We
  -- are going to want to change that once we have a better understanding of
  -- our term layer.  (That is, we should make InstF parametric in its
  -- choice of container of arguments, or we could existentially hide the
  -- container by converting InstF into a GADT.  I think either option is
  -- acceptable.)
  --
  -- Definition 3.2.18, p52, is now:
  --
  --   * @not_reached(u) === IBound u M.empty False@.
  --
  --   * @not_reached    === not_reached(UUnique)@.
  | IBound { _inst_uniq :: !Uniq
           , _inst_rec  :: !(M.Map f [i])
           , _inst_base :: !Bool
           }

  -- | This one is not in the thesis exposition but is used during
  -- computation to represent the entire universe of ground
  -- terms.  See prose, p63, \"For efficiency, we treat ground...\".
  --
  -- Defninition 3.2.17, p52 is thus rewritten: @ground(u) === IUniv u@.
  | IUniv  { _inst_uniq :: !Uniq }

  -- XXX Mercury has a concept of \"higher-order modes\", which we
  -- do not yet support (though there is grumbling about it in
  -- the background).  See 3.2.4 p55 et seq.

 deriving (Eq, F.Foldable, Functor, Ord, Show, T.Traversable)
-- $(makePrisms ''InstF)
$(makeLensesFor [("_inst_uniq","inst_uniq")
                ,("_inst_rec","inst_rec")
                ]
                ''InstF)
-- Note that makeLensesFor creates INLINE pragmas for its lenses, too. :)

instance (Eq f) => PE.Eq1 (InstF f)
-- | For the automata library's consumption; for reasoning, use lattice
-- functions.
instance (Ord f) => PE.Ord1 (InstF f)

-- | Traverse all of the recursion points @a@, rather than the @M.Map f [a]@
-- structure itself.  This provides a more robust interface to term
-- recursion, but of course loses information about disjunctions.
inst_recps :: (Applicative a) => (i -> a i') -> InstF f i -> a (InstF f i')
inst_recps = inst_rec . each . each
{-# INLINABLE inst_recps #-}

-- | An indexed variant of 'inst_recps' which reveals the functor of the
-- disjunct and the argument position being traversed right now.
inst_irecps :: (Applicative a) => ((f, Int) -> i -> a i')
                               -> InstF f i -> a (InstF f i')
inst_irecps = itraverseOf (inst_rec .> itraversed <.> itraversed)
{-# INLINABLE inst_irecps #-}

------------------------------------------------------------------------}}}
-- Instantiation States: Unary predicates                               {{{

{-
-- | Test if a term is in the set generated by the inst concretization
-- function.
--
-- Use as @term `inIGamma` inst@.
--
-- Surrogate for definition 3.2.12, p51
inIGamma :: RA (UTerm (RTerm f) v) -> Inst f -> Bool
inIGamma _        IFree          = True
inIGamma _        IBoundUniverse = True
inIGamma (RA r _) (IAny u)       = r == uniqGamma u
inIGamma (RA r t) (IBound u ts)  = r == uniqGamma u
                                   && ...
-}

-- | Extract the uniqueness from an inst.
--
-- Based on definition 3.2.13, p51 but see prose, p51, \"Note that there is
-- no uniqueness annotation on the free inst\".  This is because we assume,
-- during mode work, that we know where all the free variables are -- that
-- is, they're either uniquely referenced in some automata or they are
-- explicitly aliased.
iUniq :: InstF f i -> Maybe Uniq
iUniq = (^? inst_uniq)
{-# INLINABLE iUniq #-}

-- | Check that an instantiation state is well-formed as per defintion
-- 3.2.11, p50.
iWellFormed_ :: (Monad m)
             => (Uniq -> i -> m Bool)
             ->  Uniq -> InstF f i
             -> m Bool
iWellFormed_ _ _  IFree          = return True
iWellFormed_ _ u (IAny u')       = return $ u <= u'
iWellFormed_ q u (IBound u' b _) = if u <= u'
                                    then mfmamm (q u') b
                                    else return False
iWellFormed_ _ u (IUniv u')      = return $ u <= u'
{-# INLINABLE iWellFormed_ #-}

-- | Compare with 'IFree' without imposing @Eq i@.
iIsFree :: InstF f i -> Bool
iIsFree IFree = True
iIsFree _     = False

-- | The bottom of the 'iLeq' lattice and 'iSub' semi-lattice, representing
-- the empty set of (non-ground) terms.
iNotReached :: Uniq -> InstF f i
iNotReached u = IBound u M.empty False
{-# INLINABLE iNotReached #-}

-- | Indicator function for 'iNotReached'
--
-- Note that this is /not the same as/ asking if the inst is empty
iIsNotReached :: InstF f i -> Bool
iIsNotReached (IBound _ m False) = M.null m
iIsNotReached _                  = False
{-# INLINABLE iIsNotReached #-}

-- | Is an instantiation ground?
--
-- Surrogate for definition 3.2.17, p52.
--
-- Note that 'ground' terms include UClobbered terms; this may not be what
-- you meant.
iGround_ :: (Monad m) => (i -> m Bool) -> InstF f i -> m Bool
iGround_ q (IBound _ b _) = mfmamm q b
iGround_ _ (IUniv _) = return True
iGround_ _ _ = return False
{-# INLINABLE iGround_ #-}

{-
data IPopEst = IPEEmpty | IPESingleton | IPEMany
 deriving (Eq,Ord)

-- | An over-estimate of the number of ground terms contained within this
-- non-ground term.  iNotReached
iPopEst :: (Monad m) => (i -> m IPopEst) -> InstF f i -> m IPopEst
iPopEst _ IFree             = return IPEMany
iPopEst _ (IAny _)          = return IPEMany
iPopEst _ (IUniv _)         = return IPEMany
iPopEst _ (IBool _ _ True)  = return IPEMany
iPopEst q (IBool _ m False) = XXX
-}

------------------------------------------------------------------------}}}
-- Instantiation States: Binary predicates                              {{{

-- | Equality of two insts
iEq_ :: (Ord f, Monad m)
     => (i -> i' -> m Bool)
     -> InstF f i -> InstF f i' -> m Bool
iEq_ _ IFree IFree = return $ True
iEq_ _ _     IFree = return $ False
iEq_ _ IFree _     = return $ False

iEq_ _ (IAny u) (IAny v) | u == v = return True
iEq_ _ _        (IAny _)          = return False
iEq_ _ (IAny _) _                 = return False

iEq_ _ (IUniv u) (IUniv v) | u == v = return True
iEq_ _ _         (IUniv _)          = return False
iEq_ _ (IUniv _) _                  = return False

iEq_ q (IBound u b c) (IBound u' b' c')
  | u == u' && c == c' && (M.null $ M.difference b' b) =
  flip mapForallM b $
     \f is -> case M.lookup f b' of
                Nothing  -> return False
                Just is' -> allM $ zipWith q is is'
iEq_ _ (IBound _ _ _) (IBound _ _ _)
  | otherwise = return False
{-# INLINABLE iEq_ #-}

-- | Instantiatedness partial order with uniqueness (≼)
--
-- The purpose of iLeq is two-fold:
--   1. To describe the allowable transitions during forward execution
--      of some predicate.  (prose, p32, \"A major rôle...\").
--      Specifically, a binding may move from @i'@ to @i@ if @i `iLeq` i'@.
--   2. Defining the abstract unification lattice; see definitions
--      3.1.18 (p38), 3.2.2 (p44), 3.2.6 (p46), 3.2.19 (p53), and
--      5.3.8 (p98).
--
-- IAny `iLeq` IAny is declared (prose, p46, \"We allow...\") to be unsafe
-- in general but safe for reasoning about forward execution (recall defn
-- 3.1.10, p34).  XXX I do not understand.
--
-- Definition 3.2.14, p51 (see also definitions 3.1.4 (p32) and 3.2.5 (p46))
iLeq_ :: (Ord f, Monad m)
      => (i -> (forall i_ . InstF f i_) -> m Bool)
      -> (i -> i' -> m Bool)
      -> InstF f i -> InstF f i' -> m Bool
iLeq_ _ _  _            IFree             = return $ True
iLeq_ _ _ IFree        _                  = return $ False
iLeq_ _ _ (IAny u)     (IAny u')          = return $ u' <= u
iLeq_ _ _ (IAny _)     _                  = return $ False
iLeq_ _ _ (IUniv u)    (IAny u')          = return $ u' <= u
iLeq_ _ _ (IUniv u)    (IUniv u')         = return $ u' <= u
iLeq_ _ _ (IUniv _)    _                  = return $ False
iLeq_ q _ (IBound u b _) (IAny u')        = andM1 (u' <= u) $
    mfmamm (\x -> q x (IAny u')) b
iLeq_ q _ (IBound u b _) (IUniv u')       = andM1 (u' <= u) $
    mfmamm (\x -> q x (IUniv u')) b
iLeq_ _ q (IBound u m b) (IBound u' m' b') = andM1 (b <= b' && u' <= u) $
    flip mapForallM m $
      \f is -> case M.lookup f m' of
                 Nothing -> return False
                 Just is' -> allM $ XDU.zipWithTails q crf crf is is'
    -- XXX Ought to assert that length is == length is'
{-# INLINABLE iLeq_ #-}

-- | The type of iLeqGLB_, given a name so that we can reuse it elsewhere.
--
-- Note that not all parameters are free -- @r@ is typically @m' (InstF f
-- i'')@ for some 'Monad' @m'@ built up in terms of @m@, but the lack of
-- type lambdas forces us to resort to passing the whole type in.
type TyILeqGLB_ f m i i' i'' r = 
            (Uniq -> i  -> m i'')               -- ^ Import left
         -> (Uniq -> i' -> m i'')               -- ^ Import right
         -> (Uniq -> (forall i_ . InstF f i_) -> i  -> m i'') -- ^ Lopsided merge left
         -> (Uniq -> (forall i_ . InstF f i_) -> i' -> m i'') -- ^ Lopsided merge right
         -> (Uniq -> i -> i' -> m i'')          -- ^ Simultaneous Merge
         -> Uniq                                -- ^ Uniq of outer context
         -> InstF f i                           -- ^ Left
         -> InstF f i'                          -- ^ Right
         -> r


-- | Compute the GLB under iLeq_ (⋏) at a particular uniqueness.
--
-- The uniqueness input is needed for the case when we unify with a free
-- variable deep within a structure.  It should be UUnique at the root of
-- a unification.
--
-- Since iLeq (≼) is a lattice, this is a total function.
--
-- Notice that the \"import\" and \"merge\" callbacks take a 'Uniq'
-- argument; this 'Uniq' must be ≼ the uniqueness of the returned @i''@
-- inst in order for the produced inst to be well-formed.  It is impossible,
-- due to open recursion, for this function to engage in the necessary
-- rewrites itself.
--
-- XXX Unlike the thesis exposition, but like the Mercury implementation, we
-- might consider an alternative version of this function that returned not
-- only the result of unification, but the determinism as well.  The problem
-- is that, because we do not know the type information, we are viewing
-- insts as intersected with the type system, so we cannot actually know
-- when we might have hit notreached (we have one-sided error, but we want
-- to know the other side, as it were).  Instead, we'll use some surrogate
-- tests that are restrictive but safe (such as shallow testing for free
-- variables).

iLeqGLB_ :: (Monad m, Ord f) => TyILeqGLB_ f m i i' i'' (m (InstF f i''))
iLeqGLB_ _ r _ _ _ u IFree      x            = liftM (over inst_uniq (max u))
                                             $ T.mapM (r u) x
iLeqGLB_ l _ _ _ _ u x          IFree        = liftM (over inst_uniq (max u))
                                             $ T.mapM (l u) x

iLeqGLB_ _ _ _ _ _ _ (IAny u)   (IAny u')    = return $ IAny (max u u')

iLeqGLB_ _ _ _ _ _ _ (IAny u')  (IUniv u)    = return $ IUniv (max u u')
iLeqGLB_ _ _ _ _ _ _ (IUniv u)  (IAny u')    = return $ IUniv (max u u')
iLeqGLB_ _ _ _ _ _ _ (IUniv u)  (IUniv u')   = return $ IUniv (max u u')

iLeqGLB_ _ _ l _ _ _ (IBound u m b) (IAny u') =
 let v = max u u'
 in return . flip (IBound v) b =<< T.mapM (T.mapM (l v $ IAny v)) m
iLeqGLB_ _ _ _ r _ _ (IAny u') (IBound u m b) =
 let v = max u u'
 in return . flip (IBound v) b =<< T.mapM (T.mapM (r v $ IAny v)) m

iLeqGLB_ _ _ l _ _ _ (IBound u m b) (IUniv u') =
 let v = max u u'
 in return . flip (IBound v) b =<< T.mapM (T.mapM (l v $ IUniv v)) m
iLeqGLB_ _ _ _ r _ _ (IUniv u') (IBound u m b) =
 let v = max u u'
 in return . flip (IBound v) b =<< T.mapM (T.mapM (r v $ IUniv v)) m

iLeqGLB_ _ _ _ _ q _ (IBound u m b) (IBound u' m' b') = do
    let v = max u u'
    m'' <- mergeBoundLB (q v) m m'
    return $! IBound v m'' (b && b')
{-# INLINABLE iLeqGLB_ #-}

-- | Matches partial order with uniqueness (⊑)
--
-- Matching specifies the intersubstitutability of insts: if @i `iSub` i'@
-- then we may use anything with binding @i@ wherever the predicate would
-- require @i'@.
--
-- Note that this is not a full lattice.
--
-- Definition 3.2.15, p51 (see also definitions 3.1.11 (p35) and 3.2.7
-- (p46))
iSub_ :: (Ord f, Monad m)
      => (i -> (forall i_ . InstF f i_) -> m Bool)
      -> (i -> i' -> m Bool)
      -> InstF f i
      -> InstF f i'
      -> m Bool
iSub_ _ _ IFree          IFree          = return $ True
iSub_ _ _ x@(IBound _ _ _) IFree | iIsNotReached x = return $ True
iSub_ _ _ IFree          _              = return $ False
iSub_ _ _ _              IFree          = return $ False
iSub_ _ _ (IAny u)       (IAny u')      = return $ u <= u'
iSub_ _ _ (IAny _)       _              = return $ False
iSub_ _ _ (IUniv u)      (IAny u')      = return $ u <= u'
iSub_ _ _ (IUniv u)      (IUniv u')     = return $ u <= u'
iSub_ _ _ (IUniv _)      _              = return $ False
iSub_ q _ (IBound u b _) (IAny u')    = andM1 (u <= u') $
    mfmamm (\x -> q x (IAny u')) b
iSub_ q _ (IBound u b _) (IUniv u')   = andM1 (u <= u') $
    mfmamm (\x -> q x (IUniv u')) b
iSub_ _ q (IBound u m b) (IBound u' m' b') = andM1 (u <= u' && b <= b') $
    flip mapForallM m $
      \f is -> case M.lookup f m' of
                 Nothing -> return False
                 Just is' -> allM $ XDU.zipWithTails q crf crf is is'
    -- XXX Ought to assert that length is == length is'
{-# INLINABLE iSub_ #-}

-- | Compute the GLB under iSub_ (⊓)
--
-- Unlike 'iLeqGLB_', the \"lopsided merge\" callbacks do not need 'Uniq'
-- inputs, as the uniqueness merge operation here is 'min' and 'Uniq' is
-- required to be nondecreasing as we recurse through a term.
-- Note further the lack of \"import\" callbacks: since the only relations
-- on 'IFree' are with 'IFree' or 'iNotReached', there is no recursion to be
-- done.
iSubGLB_ :: (Ord f, Monad m)
         => ((forall i_ . InstF f i_) -> i  -> m i'')
         -> ((forall i_ . InstF f i_) -> i' -> m i'')
         -> (i -> i' -> m i'')
         -> InstF f i -> InstF f i' -> m (InstF f i'')
iSubGLB_ _ _ _ IFree      IFree        = return $ IFree
    -- The 'fromJust' calls here are safe since it is guaranteed by the
    -- above match that iUniq is being called on something that is not
    -- 'IFree' and is therefore within its domain.
iSubGLB_ _ _ _ IFree      b            = return $ iNotReached (MA.fromJust $ iUniq b)
iSubGLB_ _ _ _ a          IFree        = return $ iNotReached (MA.fromJust $ iUniq a)

iSubGLB_ _ _ _ (IAny u)   (IAny u')    = return $ IAny (min u u')

iSubGLB_ _ _ _ (IAny u')  (IUniv u)    = return $ IUniv (min u u')
iSubGLB_ _ _ _ (IUniv u)  (IAny u')    = return $ IUniv (min u u')
iSubGLB_ _ _ _ (IUniv u)  (IUniv u')   = return $ IUniv (min u u')

iSubGLB_ l _ _ (IBound u m b) (IAny u') = (return . flip (IBound (min u u')) b)
                                            =<< T.mapM (T.mapM (l (IAny u'))) m
iSubGLB_ _ r _ (IAny u') (IBound u m b) = (return . flip (IBound (min u u')) b)
                                            =<< T.mapM (T.mapM (r (IAny u'))) m

iSubGLB_ l _ _ (IBound u m b) (IUniv u') = (return . flip (IBound (min u u')) b)
                                            =<< T.mapM (T.mapM (l (IUniv u'))) m
iSubGLB_ _ r _ (IUniv u') (IBound u m b) = (return . flip (IBound (min u u')) b)
                                            =<< T.mapM (T.mapM (r (IUniv u'))) m

iSubGLB_ _ _ q (IBound u m b) (IBound u' m' b') = do
    let u'' = min u u'
    -- NOTE: I was briefly concerned that we would have to pass u'' down
    -- through q, but since the well-formedness criteria may be assumed to
    -- hold for both m and m' (that is, for all insts contained in m (resp.
    -- m'), their uniqueness is >= u (resp. u')), and our recursion is
    -- defined by minimizing uniqueness, the uniqueness of the return is
    -- guaranteed to be >= min {u, u'} with no additional effort on our
    -- part.  Contrast this with 'iLeqGLB_' and 'iSubLUB_'
    m'' <- mergeBoundLB q m m'
    return $ IBound u'' m'' (b && b')
{-# INLINABLE iSubGLB_ #-}

maxuniq :: forall a m t t' .
           (Monad m, F.Foldable t, F.Foldable t')
        => (a -> m Uniq) -> Uniq -> t (t' a) -> m Uniq

maxuniq q0 = go (go q0 UUnique)
 where go q = F.foldrM (\i u -> q i >>= return . max u)

-- | Compute the LUB under iSub_ (⊔)
--
-- Since 'iSub_' is not a full lattice, this is a partial function (thus
-- 'Maybe').  Note, however, that the recursion is defined to be total --
-- thus it is the responsibility of the outer 'Monad' to bail out if any
-- call to 'iSubGLB_' yields 'Nothing'.
--
-- Unlike the other calls, this one needs \"find the maximum 'Uniq' in an
-- Inst\" callbacks for its lop-sided merge cases.  These callbacks should
-- abort if they encounter free variables, in keeping with the defintion of
-- ⊑ .
iSubLUB_ :: (Ord f, Monad m, Show i, Show i', Show i'', Show f)
         => (i  -> m Uniq)
         -> (i' -> m Uniq)
         -> (Uniq -> i  -> m i'')
         -> (Uniq -> i' -> m i'')
         -> (i -> i' -> m i'')
         -> InstF f i -> InstF f i' -> m (Maybe (InstF f i''))
iSubLUB_ _ _ _ _ _ IFree      IFree        = return $ Just IFree
iSubLUB_ _ _ _ _ _ IFree      b     | iIsNotReached b = return $ Just IFree
iSubLUB_ _ _ _ _ _ a          IFree | iIsNotReached a = return $ Just IFree
iSubLUB_ _ _ _ _ _ IFree      _            = return $ Nothing
iSubLUB_ _ _ _ _ _ _          IFree        = return $ Nothing
                   
iSubLUB_ _ _ _ _ _ (IAny u)   (IAny u')    = return $ Just $ IAny  (max u u')
iSubLUB_ _ _ _ _ _ (IAny u')  (IUniv u)    = return $ Just $ IAny  (max u u')
iSubLUB_ _ _ _ _ _ (IUniv u)  (IAny u')    = return $ Just $ IAny  (max u u')
iSubLUB_ _ _ _ _ _ (IUniv u)  (IUniv u')   = return $ Just $ IUniv (max u u')
                   
iSubLUB_ l _ _ _ _ (IBound u m _) (IAny u') = do
  lu <- maxuniq l u m
  return (Just $ IAny (max lu u'))
iSubLUB_ _ r _ _ _ (IAny u') (IBound u m _) = do
  ru <- maxuniq r u m
  return (Just $ IAny (max ru u'))
iSubLUB_ l _ _ _ _ (IBound u m _) (IUniv u') = do
  lu <- maxuniq l u m
  return (Just $ IUniv (max lu u'))
iSubLUB_ _ r _ _ _ (IUniv u') (IBound u m _) = do
  ru <- maxuniq r u m
  return (Just $ IUniv (max ru u'))

iSubLUB_ _ _ l r q (IBound u m b) (IBound u' m' b') = do
     let v = max u u'
     m'' <- mergeBoundUB q (l v) (r v) m m'
     return $! Just $! IBound v m'' (b || b')
{-# INLINABLE iSubLUB_ #-}

------------------------------------------------------------------------}}}
-- Instantiation States: Utility Functions                              {{{

-- | const $ return False
crf :: (Monad m) => a -> m Bool
crf = const $ return False

-- | A common pattern we encounter in unary predicates, so let's just pull
-- it out.
mfmamm :: Monad m => (a -> m Bool) -> M.Map k [a] -> m Bool
mfmamm f = mapForallM (\_ -> allM . map f)

-- | Compute the lower bound of two guts of IBound constructors.
mergeBoundLB :: (Monad m, Ord f)
             => (i -> i' -> m a)
             -> M.Map f [i] -> M.Map f [i'] -> m (M.Map f [a])
mergeBoundLB q lm rm = T.sequence $ M.intersectionWith (\a b -> sequence $ zipWith q a b) lm rm

-- | Compute the upper bound of two guts of IBound constructors.
mergeBoundUB :: (Monad m, Ord f)
             => (i -> i' -> m i'')
             -> (i -> m i'')
             -> (i' -> m i'')
             -> M.Map f [i] -> M.Map f [i'] -> m (M.Map f [i''])
mergeBoundUB q l r lm rm = T.sequence
                       $ M.mergeWithKey (\_ a b -> Just $ sequence $ zipWith q a b)
                                          (fmap (T.mapM l))
                                          (fmap (T.mapM r))
                                          lm rm

------------------------------------------------------------------------}}}
