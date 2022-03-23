module Prim where
 
import Data.List
import Data.Array
 
--building the adjacency matrix -> creating az associative array

toGraph vertices graph =
    -- accumulating function
    accumArray (\xs x -> x:xs)                                       
                -- start point
                []                                                   
                -- limits
                vertices                                             
                -- add each edge twice to the matrix
                ([(x1,(x2,w)) | (x1,x2,w) <- graph, x1 /= x2] ++     
                [(x2,(x1,w)) | (x1,x2,w) <- graph, x1 /= x2])
 
 

--vertices list -> edge list

edges graph = [(v1,v2,w) | v1 <- indices graph, (v2,w) <- graph!v1]
 
prim graph = prim' [travelled] ns es []                  
    where 
        --ns = list of vertices in crescending order, travelled = starting point
        (travelled:ns) = indices graph
        --es = list of edges
        es = edges graph


--stopping point, ans = the minimal spanning tree
prim' _ [] _ ans = ans


prim' travelled next es ans = let e@(c2,u2,v2) = minimum [(c,u,v) | (u,v,c) <- es, elem u travelled, elem v next]  
                in  prim' (v2:travelled) (delete v2 next) es (e:ans) 
                          
    
-- sum of the weights of the minimum spanning tree
weightOf xs = sum[ a | ( a,_,_ ) <- xs]                               



--creating a graph  (vertex, vertex, weight of the edge)
graph1 = toGraph (1,7)                                                
             [(1,2,12),(1,3,34),(1,5,78),(2,4,55),
              (2,5,32),(2,7,4),(3,4,61),(3,5,44),
              (4,5,93),(5,6,1),(6,7,8)]        



--creating a graph  (vertex, vertex, weight of the edge)
graph2 = toGraph (1,4)                                                
             [(1,2,1011),(1,3,1000),(2,4,1000),(3,4,1)] 


--creating a graph  (vertex, vertex, weight of the edge)
graph3 = toGraph (1,9)                                                
             [(1,2,12),(1,3,34),(1,5,78),(2,4,55),                    --no route -> error
              (2,5,32),(3,4,61),(3,5,44),(4,5,93),
              (1,6,11),(6,7,8),(2,7,4),(8,9,33)]     