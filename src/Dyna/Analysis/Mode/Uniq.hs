---------------------------------------------------------------------------
-- | Reimplementation of the Mercury mode system (Uniqueness)
--
-- This module contains the definitions of uniqueness annotations and
-- primitive predicates defined by the thesis.
--
-- Much of the material here is not actually needed (and thus commented out
-- and probably a little stale) but is used in the mathematical machinery of
-- the thesis; it was implemented during a sort of rote transcription and
-- may prove useful for testing later.

-- Header material                                                      {{{
{-# OPTIONS_GHC -Wall #-}
module Dyna.Analysis.Mode.Uniq(Uniq(..)) where
------------------------------------------------------------------------}}}
-- Uniqueness Annotations                                               {{{

-- | Mercury uniqueness annotations (figure 3.15, p48)
--
-- Ord instance is ⊴ from definition 3.2.10 (p48).  Intuitively, if u1 <=
-- u2, then it is safe to use u1 in a context expecting u2.
--
-- The Mostly variants are intended (see p26) to handle trailing (i.e. undo
-- logs).  See also the discussion on p48
data Uniq = UUnique
                -- ^ All references are known to the mode analysis system.
                --
                -- In a system without alias tracking, this more obviously
                -- means \"unique reference\" (see prose, p90, \"This is a
                -- subtle change in what we mean by unique.\").
          | UMostlyUnique
          | UShared
          | UMostlyClobbered
          | UClobbered
 deriving (Bounded, Enum, Eq, Ord, Show)

{-
-- | Reference counts are bounded below by 0 or 1.
--
-- Figure 3.16, p49
data URCL = Urcl0 | Urcl1 deriving (Bounded,Enum,Eq,Ord,Show)

-- | Reference counts are bounded above by 1 or infinity.
--
-- Figure 3.16, p49
data URCH = Urch1 | UrchI deriving (Bounded,Enum,Eq,Ord,Show)
type URCC = (URCL, URCH)
newtype URC = URC (URCC, URCC) deriving (Eq,Show)

-- | Uniqueness concretization function
--
-- A section of 'uniqAlpha'.
--
-- Figure 3.16, p49
uniqGamma :: Uniq -> URC
uniqGamma UClobbered       = URC ((Urcl0, UrchI), (Urcl0, UrchI))
uniqGamma UMostlyClobbered = URC ((Urcl0, UrchI), (Urcl1, UrchI))
uniqGamma UShared          = URC ((Urcl1, UrchI), (Urcl1, UrchI))
uniqGamma UMostlyUnique    = URC ((Urcl1, Urch1), (Urcl1, UrchI))
uniqGamma UUnique          = URC ((Urcl1, Urch1), (Urcl1, Urch1))

-- | Uniqueness abstraction function
-- 
-- A retraction of 'uniqGamma'.
--
-- Figure 3.16, p49
uniqAlpha :: URC -> Uniq
uniqAlpha (URC ((Urcl0, _    ), (Urcl0, _    ))) = UClobbered
uniqAlpha (URC ((Urcl0, _    ), _             )) = UMostlyClobbered
uniqAlpha (URC ((Urcl1, Urch1), (Urcl1, Urch1))) = UUnique
uniqAlpha (URC ((Urcl1, Urch1), _             )) = UMostlyUnique
uniqAlpha (URC (_             , _             )) = UShared

-- | Partial ordering on uniqueness
--
-- p49
uniqLeq :: URC -> URC -> Bool
uniqLeq (URC (f, b)) (URC (f', b')) = f `ci` f' && b `ci` b'
  where ci (la, ua) (lb, ub) = lb <= la && ua <= ub
-}

------------------------------------------------------------------------}}}
-- Reference-count-Annotated Terms                                      {{{

{-
data RA t = RA URC t
 deriving (Eq{-F.Foldable-},Functor,Show{-,T.Traversable-})

newtype RTerm f t = RTerm { rterm :: TermF f (RA t) }
-}

------------------------------------------------------------------------}}}
