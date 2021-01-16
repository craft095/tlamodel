-------------------- MODULE MCPriorityQueue --------------------

EXTENDS TLC, PriorityQueue

CONSTANTS p1,p2,p3

gen0_MaxSize == 3
    
gen1_Tags == { p1,p2,p3 }
gen2_LessOrEqual(x,y) == LET Priorities == [[p \in Tags |-> 0] EXCEPT ![p3]=1]
        IN Priorities[x] <= Priorities[y] 

symm_gen1_Tags == Permutations(gen1_Tags)

================================================================
