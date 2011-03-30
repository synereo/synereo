{-# LANGUAGE Rank2Types #-}

-- Direct-style implementation of LogicT, using first-class delimited 
-- continuations

{- Copyright (c) 2005, Amr Sabry, Chung-chieh Shan, Oleg Kiselyov, 
	and Daniel P. Friedman
-}

-- Modified on August 2010 to migrate to the new implementation of the
-- CC monad and get rid of the 'r' parameter. The modifications let us
-- simplify msplit by inlining mplus and reflect.

module SRReifT (
  SG, runM, observe
) where

import Monad
import Control.Monad.Trans
import LogicT

-- Choose an implementation of the CC transformer
-- See http://okmij.org/ftp/continuations/

-- import CCExc
import CCCxe

-- Obsolete implementation:
-- import CC_2CPST
-- import CC_FrameT

-- -------------------------------------------------------------
-- The datatype Tree reifies the effect (non-determinism)

data Tree m a = HZero | HOne a | HChoice a (TreeM m a)
type TreeM m a = CC (PS (Tree m a)) m (Tree m a)

-- Like function composition but has the benefit of rotating the 
-- tree as it proceeds

compose_trees :: Monad m => Tree m a -> TreeM m a -> TreeM m a
compose_trees HZero r = r
compose_trees (HOne a) r = return $ HChoice a r
compose_trees (HChoice a r') r = return $ 
				 HChoice a $ r' >>= (\v -> compose_trees v r)

{-
treefold :: (a -> b -> b) -> (a -> b) -> b -> Tree a -> b
treefold kons kone knil HZero = knil
treefold kons kone knil (HOne a) = kone a
treefold kons kone knil (HChoice a r) = 
    kons a (treefold kons kone knil r)
-}
-- -------------------------------------------------------------
type SG m a = SR m a

newtype SR m a = SR{unSR:: forall ans. CC (PS (Tree m ans)) m a}


-- Since SR is the newtype, the SR monad is just as efficient as CC
instance Monad m => Monad (SR m) where
  return x = SR $ return x
  m >>= f  = SR $ unSR m >>= (unSR . f)


instance Monad m => MonadPlus (SR m) where
  mzero = SR $ abortP ps (return HZero)
  -- Do not write the second argument of mplus as (SR m2)
  -- That forces the evaluation of m2, unnecessarily!
  m1 `mplus` m2 = SR $ (id =<<) . shift0P ps $ \sk -> 
                         do f1    <- sk (unSR m1)
                            let f2 = sk (unSR m2)
			    compose_trees f1 f2

instance MonadTrans SR where
    lift m = SR (lift m)

instance (MonadIO m) => MonadIO (SR m) where
    liftIO = lift . liftIO

instance LogicT SR where
    msplit m = SR (lift $ (runCC (reify m) >>= (return . reflect_sr)))
	where reflect_sr HZero          =  Nothing
	      reflect_sr (HOne a)       = Just (a, mzero)
	      reflect_sr (HChoice a r1) = Just (a, refl (runCC r1))

refl :: Monad m => m (Tree m a) -> SR m a
refl m = SR (lift m >>= check)
 where                 -- we inline mzero and mplus
 check HZero         = abortP ps (return HZero)
 check (HOne a)      = return a
 check (HChoice a r) = (id =<<) . shift0P ps $ \sk ->
		         do f1 <- sk (return a)
			    let f2 = sk (unSR . refl $ runCC r)
			    compose_trees f1 f2


reify :: Monad m => SR m a -> TreeM m a
reify m = pushPrompt ps (unSR m >>= (return . HOne))

runM :: Monad m => Maybe Int -> SR m a -> m [a]
runM n m = runCC (reify m >>= flatten n)
  where 
  flatten _ HZero = return []
  flatten (Just n) _ | n <= 0 = return []
  flatten _ (HOne a) = return [a]
  flatten (Just 1) (HChoice a r) = return [a] -- Don't run r unless needed!
  flatten n (HChoice a r) = r >>= flatten (fmap pred n) >>= (return . (a:))

-- Hinze's `observe' -- the opposite of `lift'
--	observe . lift == id

observe :: Monad m => SR m a -> m a
observe m = runCC (reify m) >>= pick1
  where pick1 HZero         = fail "no anwers"
	pick1 (HOne a)      = return a
	pick1 (HChoice a _) = return a

