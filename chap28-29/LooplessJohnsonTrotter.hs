module LooplessJohnsonTrotter (johnsonTrotter) where

-- Bird, Richard (2010)
-- Pearls of Functional Programming Design. Cambridge University Press, 2010
-- Chapter 29: The Johnson-Trotter Algorithm

import Data.List (unfoldr)

import OkasakiQueue

type Forest a  =  Queue (Rose a)
data Rose a  =  Node a (Forest a, Forest a) -- deriving Show
type State = (Int,Int,Int,Int,Int)
type Pair a = (a,a)

-- my addition: the main procedure (GR)
johnsonTrotter n  = jcode (0,n)

jcode  =  unfoldr (step) . prolog

prolog =  wrapQueue . fst . foldr op' (empty,empty) . pairs

op':: (Int,Int) -> Pair (Forest (Int,State))-> Pair (Forest (Int,State))
op' (k,n) (ys,sy)  =  if odd n
 then (mix (-1,k,k,n-1,1) (ys,sy), mix (1,0,k,1,n-1) (sy,ys))
 else (mix (-1,k,k,n-1,1) (ys,sy), mix (1,k,k,1,n-1) (ys,sy))

mix:: State -> Pair (Forest (Int,State))-> Forest (Int,State)
mix (i,j,k,m,n) (ys,sy)
   = if i*(n-m)<0 then ys
     else insert ys (Node (m+j, (i,k-j,k,m+i,n)) (ys,sy))

pairs:: (Int,Int)-> [(Int,Int)]
pairs (k,n) = addpair (k,n) []
addpair (k,1) ps = ps
addpair (k,n) ps = addpair (k',n-1) ((k,n):ps)
        where k' = if odd n then k+1 else 1

wrapQueue:: Queue a -> [Queue a]
wrapQueue xs  =  consQueue xs [] 

consQueue:: Queue a -> [Queue a] -> [Queue a]
consQueue xs xss  =  if isempty xs then xss else xs:xss

step:: [Forest (Int,State)]-> Maybe (Int, [Forest (Int,State)])
step []        =  Nothing
step (zs:zss)  =  Just (x,consQueue (mix q (sy,ys)) (consQueue zs' zss))
                  where (Node (x,q) (ys,sy), zs')  =  remove zs

