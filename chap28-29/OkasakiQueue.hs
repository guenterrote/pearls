module OkasakiQueue (Queue,empty,isempty,insert,remove) where

-- CHRIS OKASAKI
-- Simple and Efficient Purely Functional Queues and Deques
-- J. Functional Programming 5 (4): 583-592, Section 5

type Queue a  =  ([a],[a],[a])

insert::  Queue a -> a -> Queue a
remove::  Queue a -> (a, Queue a)
empty::   Queue a
isempty:: Queue a -> Bool


empty  =  ([],[],[])
isempty (ls,_,_)  =  null ls

-- swapped the order of arguments w.r.t. the paper:
insert (ls,rs,l's) e   =  makeeq (ls,e:rs,l's)

remove (l:ls,rs,l's)   =  (l, makeeq (ls,rs,l's))

makeeq (ls,rs,[])      =  let l's = rot (ls,rs,[]) in (l's,[],l's)
makeeq (ls,rs,_:l's)   =  (ls,rs,l's)

rot ([],    r:_,   as) =  r : as
rot ((l:ls),(r:rs),as) =  l : rot (ls,rs,r:as)
