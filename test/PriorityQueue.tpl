MODULE PriorityQueue;

CONSTANTS 
    MaxSize <- 3
    Tags <- [model value] <symmetrical> {p1, p2, p3}
    LessOrEqual(x,y) <-
        LET Priorities == [[p \in Tags |-> 0] EXCEPT ![p3]=1]
        IN Priorities[x] <= Priorities[y] ;

SPECIFICATION Spec ;

CHECK_DEADLOCK FALSE ;

INVARIANT TypeOk ;
