-- Implementation of LogicM with streams modeled as (lazy) lists

{- Copyright (c) 2005, Amr Sabry, Chung-chieh Shan, Oleg Kiselyov, 
	and Daniel P. Friedman
-}

module ListLM where

import Control.Monad
import LogicM

newtype SSG a = Stream [a]
unSSG (Stream str) = str

instance Monad SSG where
  return e = Stream [e]
  (Stream es) >>= f = 
      Stream (concat (map (unSSG . f) es))

instance MonadPlus SSG where
  mzero = Stream []
  (Stream es1) `mplus` (Stream es2) = 
      Stream (es1 ++ es2)

instance LogicM SSG where
  msplit (Stream [])     =   return Nothing
  msplit (Stream (h:t))  =   return $ Just (h, Stream t)
