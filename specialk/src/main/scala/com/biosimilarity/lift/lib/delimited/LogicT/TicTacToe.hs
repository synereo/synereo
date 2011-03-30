{-# LANGUAGE KindSignatures, PatternGuards #-}

-- The TicTacToe problem for the NxN board
-- M consecutive 'X' or 'O' marks in each column, row, or diagonal
-- is a win for the corresponding player
-- This code is heavily based on the code posted by Andrew Bromage on
-- the Haskell mailing list on June 22, 2005:
-- http://www.haskell.org/pipermail/haskell/2005-June/016037.html

-- To compile this code
-- ghc --make -O2 -main-is a12a1 TicTacToe.hs
-- To run this code
-- GHCRTS="-tstderr" /usr/bin/time ./TicTacToe

module Main where

import Control.Monad
import Control.Monad.Trans
import LogicT
import qualified Data.Map as Map
import Data.List

-- import SFKT		-- 2-CPS style
import SRReifT		-- Direct-style, with shift/reset


n = 5				-- Dimension of the board
m = 4				-- Number of consecutive marks needed for win

-- ----------------------------------------------------------------------
--			Representation of the board

-- We also use Mark to mark the players
data Mark = X | O deriving (Ord,Eq,Show)

-- Location on the board: 0..n-1
type Loc = (Int,Int)

-- The current position: What Mark is at Loc. Initially empty
type Board = Map.Map Loc Mark


-- Determining the number of consecutive marks around the location loc
-- (in a particular or all directions)

-- movement functions: Loc -> Loc from one location to the neighboring one.
-- For each direction (row, column, diagonal) we provide two
-- functions: one moves into the positive direction, and
-- the other in the negative direction

type MoveFn = Loc -> Loc
move'loc'fn :: [(MoveFn,MoveFn)]
move'loc'fn =
    [(\ (x,y) -> (x-1,y) ,  \ (x,y) -> (x+1,y)), -- up/down the column y
     (\ (x,y) -> (x,y-1) ,  \ (x,y) -> (x,y+1)), -- left/right the row x
     (\ (x,y) -> (x-1,y-1), \ (x,y) -> (x+1,y+1)), 
     (\ (x,y) -> (x-1,y+1), \ (x,y) -> (x+1,y-1))]

good'loc (x,y) = x>=0 && y>=0 && x<n && y<n

-- move as far as possible from the location loc into the direction specified
-- by mfn so long as new location is still marked by 'm'. Return the
-- last location marked by 'm' and the number of the moves performed.
extend'loc :: Board -> MoveFn -> Mark -> Loc -> (Int,Loc)
extend'loc board mfn m loc = loop 0 loc (mfn loc)
    where loop n loc loc' | good'loc loc',
	                    Just m' <- Map.lookup loc' board,
			    m' == m 
		      = loop (n+1) loc' (mfn loc')
	  loop n loc _ = (n,loc)

max'cluster :: Board -> Mark -> Loc -> (Int,Loc)
max'cluster board m loc = maximumBy (\ (n1,_) (n2,_) -> compare n1 n2) $
			            (map cluster'dir move'loc'fn)
    where cluster'dir (mfn1,mfn2) = 
	      let (n1,end1) = extend'loc board mfn1 m loc
	          (n2,end2) = extend'loc board mfn2 m loc
	      in (n1+n2+1,end1)


-- The current position of the game then

data Game = Game {
		  -- The location and the mark of the
		  -- player who first achieved the goal
		  winner :: Maybe (Loc,Mark),
		  -- The list of empty locations
		  moves  :: [Loc],
		  board  :: Board
		  }

new'game :: Game
new'game = Game { winner = Nothing,
		  moves = map (\[x,y] ->(x,y)) $ sequence [[0..n-1],[0..n-1]],
		  board = Map.empty}

show'board fm = concatMap showrow [0..n-1]
    where showrow i = concatMap (showmark i) [0..n-1] ++ "\n"
	  showmark i j = maybe " ." ((' ':) . show) $ Map.lookup (i,j) fm


-- Account for the move into location the 'loc' by the player 'p'

take'move :: Mark -> Loc -> Game -> Game
take'move p loc g = 
    Game { moves = delete loc (moves g),
	   board = board',
	   winner = let (n,l) = max'cluster board' p loc
                    in if n >= m then Just (l,p) else Nothing
	 }
  where
     board' = Map.insert loc p (board g)


-- The main game-playing function

type PlayerProc t (m :: * -> *) = Mark -> Game -> t m (Int,Game)
game :: (MonadPlus (t m), LogicT t, Monad m, MonadIO (t m)) =>
	(Mark,PlayerProc t m) -> (Mark,PlayerProc t m) -> t m ()
game player1 player2
     = game' player1 player2 new'game
     where
         game' player@(p,proc) other'player g
             | Game{winner=Just k} <- g
                 = liftIO (putStrLn $ (show k) ++ " wins!")
             | Game{moves=[]} <- g
                 = liftIO (putStrLn "Draw!")
             | otherwise
                 = do
                     (_,g') <- once (proc p g)
                     liftIO (putStrLn $ show'board (board g'))
                     game' other'player player g'


-- Play as a human
human'player :: (MonadIO (t m), MonadTrans t) => PlayerProc t m
human'player p g = do
    liftIO $ (putStrLn $ "Your (i,j) move as " ++ (show p))
    let loop = liftIO getLine >>= 
	       \s -> case (reads s) of
			[(l,"")] -> return l
			_ -> (liftIO $ putStrLn "Parse Error") >> loop
    l@(i,j) <- loop
    if elem l (moves g) then return (1,(take'move p l g))
       else (liftIO $ putStrLn "Bad Move") >> human'player p g

h2h :: IO ()
h2h = observe $ game (X,human'player) (O,human'player)

-- ----------------------------------------------------------------------
--				Heuristics

{-
Andrew Bromage wrote:
This is a simple problem in AI.  Basically, you're trying to do a minimax
search.  If this is a "goal state" (i.e. an ACTUAL win, lose or draw), then
we're done.  If not, we examine all of the successor states, assuming that
our opponent will make their best move, and we pick the one that's best
for us.
-}

ai :: (MonadPlus (t m), Monad m, LogicT t) => PlayerProc t m
ai p g
    | Game{winner=Just _} <- g
        = return (estimate'state p g,g)
    | Game{moves=[]} <- g
        = return (estimate'state p g,g)
    | otherwise
        = do
            wbs <- bagofN (Just 5) (do
                m   <- choose (moves g)
                let g' = take'move p m g
                (w,_) <- ai (other'player p) g'
                return (-w,g'))
            let (w,g') = maximumBy (\ (x,_) (y,_) -> compare x y) wbs
            return (w,g')


-- Utility "axiom of choice" function:
-- A more sophisticated choice functions are possible
choose :: (MonadPlus m) => [a] -> m a
choose = msum . map return

other'player X = O
other'player O = X

-- the more the better
estimate'state :: Mark -> Game -> Int
estimate'state p g 
    | Game{winner=Just (_,p')} <- g
        = if p == p' then score'win  else score'lose
    | Game{moves=[]} <- g
        = 0				-- draw
    | otherwise = 10
score'win = maxBound
score'lose = - maxBound

a2a, a2h :: IO ()
a2a = observe $ game (X,ai) (O,ai)

a2h = observe $ game (X,human'player) (O,ai)

{-
Andrew Bromage wrote:
Unfortunately, this is too slow for interactive use.  Certainly, I ran
out of patience after a minute.  However, thankfully there are a couple
of safe heuristics which work just fine with tic-tac-toe.

The first is that if you can win in this move, you should do so.

The second is that if the first heuristic doesn't work, then you should
see if there is any move that your opponent could make where they could
win on the next move.  If so, you should move to block it.
-}

first'move'wins p g =
    do
    m <- choose (moves g)
    let g' = take'move p m g
    guard (maybe False (\ (_,p') -> p' == p) (winner g'))
    return (m,(score'win,g'))


minmax :: (MonadPlus (t m), Monad m, LogicT t) =>
	  (Int->Int->PlayerProc t m) -> (Int->Int->PlayerProc t m)
minmax self dlim blim p g =
    do
    wbs <- bagofN (Just blim)
	   (do
            m   <- choose (moves g)
            let g' = take'move p m g
	    if dlim <= 0 then return (estimate'state p g',g')
	       else do (w,_) <- self (dlim-1) blim 
					(other'player p) g'
		       return (-w,g'))
    let (w,g') = maximumBy (\ (x,_) (y,_) -> compare x y) wbs
    return (w,g')

ai' :: (MonadPlus (t m), Monad m, LogicT t) => PlayerProc t m
ai' p g = ai'lim m 6 p g
  where 
  ai'lim dlim blim p g
    | Game{winner=Just _} <- g
        = return (estimate'state p g,g)
    | Game{moves=[]} <- g
        = return (estimate'state p g,g)
    | otherwise
        = ifte (once (first'move'wins p g))
          (return . snd)
            (ifte (once (first'move'wins (other'player p) g))
              (\ (m,_) -> do
	          let g' = take'move p m g
                  (w,_) <- ai'lim dlim blim (other'player p) g'
                  return (-w,g'))
	      (minmax ai'lim dlim blim p g))

-- Playing the computer against the computer.
-- The game start-ups slowly, please be patient. It speeds up after
-- the first two moves
a12a1, a12h, h2a1 :: IO ()
a12a1 = observe $ game (X,ai') (O,ai')

a12h = observe $ game (X,human'player) (O,ai')
h2a1 = observe $ game (O,ai') (X,human'player) 

{- Performance (median of 5 runs), GHC 6.10

SFKT monad
./TicTacToe +RTS -tstderr 
<<ghc: 1064319528 bytes, 2031 GCs, 402911/807968 avg/max bytes residency (43 samples), 3M in use, 0.00 INIT (0.00 elapsed), 2.94 MUT (3.00 elapsed), 0.64 GC (0.65 elapsed) :ghc>>
        3.65 real         3.57 user         0.06 sys

SRReifT with CC_FrameT
./TicTacToe +RTS -tstderr 
<<ghc: 3059137064 bytes, 5836 GCs, 399535/809024 avg/max bytes residency (33 samples), 3M in use, 0.00 INIT (0.00 elapsed), 7.19 MUT (7.39 elapsed), 0.89 GC (0.89 elapsed) :ghc>>
        8.28 real         8.07 user         0.14 sys

SRReifT with CC_2CPST
<<ghc: 4636813080 bytes, 8845 GCs, 382786/738812 avg/max bytes residency (37 samples), 3M in use, 0.00 INIT (0.00 elapsed), 10.50 MUT (10.89 elapsed), 1.21 GC (1.23 elapsed) :ghc>>
       12.12 real        11.70 user         0.32 sys

SRReifT with CCExc
<<ghc: 2221188964 bytes, 4237 GCs, 378359/795220 avg/max bytes residency (34 samples), 3M in use, 0.00 INIT (0.00 elapsed), 7.19 MUT (7.31 elapsed), 0.75 GC (0.76 elapsed) :ghc>>
        8.07 real         7.94 user         0.09 sys

SRReifT with CCCxe
<<ghc: 1938323968 bytes, 3698 GCs, 360827/754936 avg/max bytes residency (28 samples), 3M in use, 0.00 INIT (0.00 elapsed), 5.04 MUT (5.18 elapsed), 0.61 GC (0.62 elapsed) :ghc>>
        5.80 real         5.65 user         0.12 sys

SVCT with CC_FrameT
<<ghc: 3485808596 bytes, 6649 GCs, 888914/1833404 avg/max bytes residency (42 samples), 5M in use, 0.00 INIT (0.00 elapsed), 8.11 MUT (8.34 elapsed), 1.42 GC (1.44 elapsed) :ghc>>
        9.78 real         9.53 user         0.17 sys


SVCT with CC_2CPST (2 runs)
<<ghc: 5310665320 bytes, 10130 GCs, 951510/2138500 avg/max bytes residency (42 samples), 6M in use, 0.00 INIT (0.00 elapsed), 11.62 MUT (11.95 elapsed), 1.79 GC (1.83 elapsed) :ghc>>
       13.78 real        13.41 user         0.29 sys


SVCOT with CC_FrameT (2 runs)
<<ghc: 3716627796 bytes, 7090 GCs, 1229370/2859200 avg/max bytes residency (41 samples), 8M in use, 0.00 INIT (0.00 elapsed), 8.55 MUT (8.78 elapsed), 1.75 GC (1.81 elapsed) :ghc>>
       10.59 real        10.30 user         0.21 sys


SVCOT with CC_2CPST
<<ghc: 5711552100 bytes, 10895 GCs, 1159999/2912320 avg/max bytes residency (47 samples), 8M in use, 0.00 INIT (0.00 elapsed), 12.76 MUT (13.07 elapsed), 2.26 GC (2.30 elapsed) :ghc>>
       15.37 real        15.02 user         0.26 sys

-}
