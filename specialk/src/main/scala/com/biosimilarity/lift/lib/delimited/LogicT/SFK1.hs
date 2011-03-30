{-# LANGUAGE Rank2Types #-}
-- Implementation of LogicM based on two-continuation model of streams

{- Copyright (c) 2005, Amr Sabry, Chung-chieh Shan, Oleg Kiselyov, 
	and Daniel P. Friedman
-}

module SFK1 (
  SG, runM
) where

import Control.Monad
import LogicM

type SG a = SFK a

newtype SFK a  = SFK{unSFK:: forall ans. SK ans a -> FK ans -> ans}

type FK ans = ans
type SK ans a = a -> FK ans -> ans

instance Monad SFK where
  return e = SFK (\sk fk -> sk e fk)
  (SFK m) >>= f = 
      SFK (\sk fk -> 
           m (\a fk' -> unSFK (f a) sk fk') 
             fk)

instance MonadPlus SFK where
  mzero = SFK (\_ fk -> fk)
  (SFK m1) `mplus` (SFK m2) = SFK (\sk fk -> m1 sk (m2 sk fk))

instance LogicM SFK where			    
    msplit (SFK m) = m ssk caseB 
	where caseB = return $ Nothing 
	      caseA h t = return $ Just (h, t)
	      ssk sub fk = caseA 
			   sub
			   (do r <- fk
			       case r of
			            Nothing -> mzero
			            Just (sg1, sgr) -> 
			                      (return sg1) `mplus` sgr)

runM :: SG a -> [a]
runM (SFK m) = m (:) [] 

