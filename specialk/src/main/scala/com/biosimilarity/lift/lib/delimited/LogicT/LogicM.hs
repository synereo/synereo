
-- Logic Monad: MonadPlus with interleave, bindi, ifte and once
-- Definition and implementation of generic operations, in terms of msplit

{- Copyright (c) 2005, Amr Sabry, Chung-chieh Shan, Oleg Kiselyov, 
	and Daniel P. Friedman
-}


module LogicM (
  LogicM(..)
) where 

import Monad

class MonadPlus m => LogicM m where
    msplit :: m a -> m (Maybe (a, m a))

    -- All other things are implemented in terms of MonadPlus + msplit
    -- All the functions below have generic implementations

    -- fair disjunction
    interleave :: m a -> m a -> m a
    -- Note the generic implementation below
    -- The code is *verbatim* from Logic.hs
    interleave sg1 sg2 =
	do r <- msplit sg1 
	   case r of 
		  Nothing -> sg2
		  Just (sg11,sg12) -> (return sg11) `mplus` 
				      (interleave sg2 sg12)

    -- just conventional aliases
    gsuccess:: a -> m a
    gsuccess = return
    gfail :: m a
    gfail = mzero

    -- standard `bind' is the conjunction
    -- the following is a fair conjunction
    -- Again, the old Logic.hs code works verbatim
    bindi:: m a -> (a -> m b) -> m b
    bindi sg g = do r <- msplit sg 
		    case r of 
                       Nothing -> mzero
                       Just (sg1,sg2) ->  interleave 
                                            (g sg1)
                                            (bindi sg2 g)

    -- Pruning things

    -- Soft-cut (aka if-then-else)
    -- ifte t th el = (or (and t th) (and (not t) el))
    -- However, t is evaluated only once
    ifte :: m a -> (a -> m b) -> m b -> m b
    ifte t th el = 
	do r <- msplit t 
	   case r of 
		  Nothing -> el
		  Just (sg1,sg2) -> (th sg1) `mplus` (sg2 >>= th)

    once :: m a -> m a
    once m = 
	do r <- msplit m
	   case r of 
		  Nothing -> mzero
		  Just (sg1,_) -> return sg1


