-- | Deterministic string automata: each (state, symbol) pair transitions
-- uniquely to a new state.
data DFAF alphabet state = M.Map alphabet state

-- | An example for describing non-deterministic string automata: each
-- (state,symbol) pair transitions to a set of possible successor states.
-- This formulation does not allow for \epsilon edges but requires that we
-- compute epsilon closure of the transition relation.
data NFAF alphabet state = M.Map alphabet [state]
