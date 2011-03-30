-- LogicT sample code
-- In particular: Missionaries and cannibals problem

{-
  Copyright (c) 2005, Amr Sabry, Chung-chieh Shan, Oleg Kiselyov, 
	and Daniel P. Friedman

  $Id: MCPT.hs,v 1.15 2005/09/12 22:40:09 oleg Exp $

-}

module MCPT where

import Monad
import Control.Monad.Identity
import Control.Monad.Trans

import LogicT

-- Choose one of the two following implementations of the LogicT monad

-- import SFKT		-- 2-CPS style
import SRReifT		-- Direct-style, with shift/reset

-- The following two are obsolete. FBackTrack performs much better
-- import SVCT		-- Direct-style, with supervisor control
-- import SVCOT		-- Direct-style, with supervisor control, optimized


-------------------------------------------------------------------------------
-- Given M missionaries, C cannibals, and B boats where each boat 
--   holds at most two people
-- Goal: move M+C from one side to the other
-- Constraint: cannibals never outnumber missionaries in any place
-- For more details on the problem, see:
-- http://www.cs.berkeley.edu/~russell/code/search/domains/cannibals.lisp


-- (M,C,B) on each side of the river
type Left  = (Int,Int,Int)
type Right = (Int,Int,Int)
type State = (Left,Right)

-- A final state
final :: MonadPlus m => State -> m State
final s@((0,0,_),_) = return s
final _ = mzero

-- Moves

data MoveDir = FWD | BWD		-- From left-to-right or vice versa
	     deriving Show

-- An action: a change in the State
type Action = (Int,Int,MoveDir)

-- Permissible actions: at most two people can move in a boat
legalActions :: [Action]
legalActions = (map (add_dir FWD) changes) ++ (map (add_dir BWD) changes)
    where 
    changes = [(1,0),(0,1),(2,0),(0,2),(1,1)]
    add_dir dir (a,b) = (a,b,dir)

-- The transmission function...
-- Apply an action to a state. Fail if a bad state is reached
move :: MonadPlus m => State -> Action -> m State
move ((m1,c1,b1),(m2,c2,b2)) (mm,cm,FWD) | b1 > 0 = 
  check ((m1-mm, c1-cm, b1-1),(m2+mm, c2+cm, b2+1))
move ((m1,c1,b1),(m2,c2,b2)) (mm,cm,BWD) | b2 > 0 = 
  check ((m1+mm, c1+cm, b1+1),(m2-mm, c2-cm, b2-1))
move _ _ = mzero

-- Check the newly reached state. Fail if it is bad
check :: MonadPlus m => State -> m State
check s@((m1,c1,b1),(m2,c2,b2)) = 
    if and [m1 >= 0, m2 >= 0, c1 >= 0, c2 >= 0, 
	    (m1 == 0 || c1 <= m1),	-- If there are missionaries, there
					-- should be at least as many 
	    (m2 == 0 || c2 <= m2)]	-- of them as there are cannibals
       then return s
       else mzero


-- non-deterministically, choose an element from a list
-- Obviously, we fail if the list is empty.
-- This function is obviously a manifestation of the Axiom of Choice
choose:: MonadPlus m => [a] -> m a
choose = msum . map return

occurs e lst = do { e' <- choose lst; if e == e' then return e else mzero }

-- The first solution: Depth-first-search

data SearchS = SearchS State     -- Current state
	               [State]   -- Seen states; includes current
		       [Action]  -- Actions that lead to the current state

instance Show SearchS where
    show (SearchS _ _ actions) = show actions

solve_dfs (SearchS current seen actions) = 
    do 
    a     <- choose legalActions
    s     <- move current a
    liftIO $ putStrLn $ "Tentative move: " ++ (show current) ++ " -" ++
	                (show a) ++ "-> " ++ (show s)
    let news = SearchS s (s:seen) (a:actions)
    ifte (final s) 
	 (const $ return news)
	 (ifte (once (occurs s seen))
	       (const $ mzero)
	       (solve_dfs news))


do'solve left = result >>= mapM_ print 
      where s = (left, (0,0,0))
	    result = observe (bagofN Nothing $ solve_dfs (SearchS s [s] []))

{- Sample output. The last action is shown first.
do'solve (3,3,1)
[(1,1,FWD),(1,0,BWD),(0,2,FWD),(0,1,BWD),(2,0,FWD),(1,1,BWD),(2,0,FWD),
 (0,1,BWD),(0,2,FWD),(0,1,BWD),(0,2,FWD)]
[(0,2,FWD),(0,1,BWD),(0,2,FWD),(0,1,BWD),(2,0,FWD),(1,1,BWD),(2,0,FWD),
 (0,1,BWD),(0,2,FWD),(0,1,BWD),(0,2,FWD)]
[(1,1,FWD),(1,0,BWD),(0,2,FWD),(0,1,BWD),(2,0,FWD),(1,1,BWD),(2,0,FWD),
 (0,1,BWD),(0,2,FWD),(1,0,BWD),(1,1,FWD)]
[(0,2,FWD),(0,1,BWD),(0,2,FWD),(0,1,BWD),(2,0,FWD),(1,1,BWD),(2,0,FWD),
 (0,1,BWD),(0,2,FWD),(1,0,BWD),(1,1,FWD)]

do'solve (4,4,1)
has no solutions.
-}


-------------------------------------------------------------------------------
--		Simple tests of LogicT monad operations

-- Simple test of interleaving

t0, t1, t2 :: MonadPlus m => m Int
t0 = mzero
t1 = (return 10) `mplus` (return 20) `mplus` (return 30)
t2 = msum (map return [10,20,30])

-- Specifically for list Monad
runList :: [a] -> [a]
runList = id

to :: MonadPlus m => m Int
to = (return 1) `mplus` (to >>= (return . (2 +)))

runL :: Maybe Int -> SG Identity a -> [a]
runL n m = runIdentity $ observe (bagofN n m)
test0 = runL (Just 5) to

-- Note! we can't write runL n $ expr
-- we must write runL n (expr)
-- due to the higher-rank type of runL...

-- Starvation of t2
test1 = runL (Just 10) (to `mplus` t2)
test1' = runL (Just 1) ( 
     do  x <- to `mplus` t2
	 if even x then return x else mzero)


-- No starvation
test2 = runL (Just 10) (to `interleave` t2)

test2' = runL (Just 1) ( (to `interleave` t2) >>= 
			  (\x->if even x then return x else mzero))

test3 = runL Nothing (t2 `mplus` t2)
test4 = runL Nothing (interleave t2 t2)

-- The need for bindi
test5 = let k n = to >>= (return . (n +)) in 
        take 1 $ runList ((return 0) `mplus` (return 1)) >>= k >>=
		        (\x ->if even x then return x else mzero)
test5' = let k n = to >>= (return . (n +)) in 
         runL (Just 1) ( ((return 0) `mplus` (return 1)) `bindi` k >>=
		          (\x ->if even x then return x else mzero) )

-- Showing non-strict associativity of bindi

f1 x = (return (x+1) `mplus` return (x+2))
f2 x = (return (x+100) `mplus` return (x+200))

test_bind = runL Nothing ((t2 >>= f1) >>= f2)
test_bind' = runL Nothing (t2 >>= (\x -> f1 x >>= f2))

test_bindi = runL Nothing (bindi (bindi t2 f1) f2)
test_bindi' = runL Nothing (bindi t2 (\x -> bindi (f1 x) f2))

test_bindr = runL (Just 10) (to >>= return) == runL (Just 10) to

-- The need for the logical if-then-else

-- Sequence of odd non-primes

iota n = if n < 1 then mzero else return n `mplus` iota (n-1)

test_oc = runL (Just 10) (
	         do
		   n <- to
		   guard $ n > 1
		   d <- iota (n-1)
		   guard $ d > 1 && n `mod` d == 0
		   return n)


test_op = runL (Just 10) (
	         do
		   n <- to
		   guard $ n > 1
		   ifte (do
			 d <- iota (n-1)
			 guard $ d > 1 && n `mod` d == 0
			 -- _ <- trace ("d: " ++ show d) (return ())
			 return d)
		      (\_->mzero)
		      (return n))

test_opio = print =<< (observe (bagofN (Just 10) $
	    do
	    n <- to
	    guard $ n > 1
	    ifte (do
		  d <- iota (n-1)
		  guard $ d > 1 && n `mod` d == 0
		  liftIO $ putStrLn ("d: " ++ show d)
		  return d)
		(\_->mzero)
		(return n)))


test_op' = runL (Just 10) (
                 do
		   n <- to
		   guard $ n > 1
		   ifte (once (do
			       d <- iota (n-1)
			       guard $ d > 1 && n `mod` d == 0
			       --_ <- trace ("d: " ++ show d) (return ())
			       return d))
		      (\_->mzero)
		      (return n))
			  
-- Slowsort

-- generate permutations

member e [] = return [e]
member e l@(h:t) = return (e:l) `mplus` do { t' <- member e t; return (h:t') }

permute [] = return []
permute (h:t) = do { t' <- permute t; member h t' }

slowsort l = permute l >>= (\l -> if check l then return l else mzero)
    where check (e1:e2:r) = e1 <= e2 && check (e2:r)
	  check _ = True

test_ss = runL Nothing (slowsort [5,0,3,4,0,1])
-- [[0,0,1,3,4,5],[0,0,1,3,4,5]]

slowsort' l = once (permute l >>= (\l -> if check l then return l else mzero))
    where check (e1:e2:r) = e1 <= e2 && check (e2:r)
	  check _ = True
test_ss' = runL Nothing (slowsort' [5,0,3,4,0,1])

-- Tests from Spivey2006, `Algebras for combinatorial search'.
-- bindi per se does not assure completeness of search
-- See FBackTrack.hs instead, which has a more complex example
-- of interleaving three infinite streams.
stream_from n = return n `interleave` (stream_from (n+1))

test_stream  = runL (Just 10) (stream_from 2)
-- [2,3,4,5,6,7,8,9,10,11]

-- The test at the end of p7 of Spivey2006, in desugared notation
test_sp1 = runL (Just 1) (
            stream_from 2 >>= 
	     (\a -> (stream_from 2 >>= 
		     (\b -> if a*b == 9 then return (a,b) else mzero))))
-- diverges

test_sp2 = runL (Just 1) (
            stream_from 2 `bindi` 
	     (\a -> (stream_from 2 `bindi`
		     (\b -> if a*b == 9 then return (a,b) else mzero))))
