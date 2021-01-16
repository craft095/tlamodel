--------------------- MODULE PriorityQueue --------------------------

EXTENDS Sequences, Integers

CONSTANTS Tags, MaxSize, LessOrEqual(_,_)

ASSUME MaxSize \in Nat

\* Totality (and reflexivity)
ASSUME \A t0, t1 \in Tags :
    LessOrEqual(t0, t1) \/ LessOrEqual(t1, t0)
\* Transitivity
ASSUME \A t0, t1, t2 \in Tags :
    (LessOrEqual(t0, t1) /\ LessOrEqual(t1, t2)) => LessOrEqual(t0, t2)

VARIABLES queue, op

vars == <<queue, op>>

OpNames == { "init", "put", "get" }

TypeOk ==
    /\ \E n \in 0..MaxSize : queue \in [1..n -> Tags]
    /\ op \in (OpNames \X Tags \X BOOLEAN)

Init ==
    /\ \E n \in 0..MaxSize : queue \in [1..n -> Tags]
    /\ op = <<"init", CHOOSE x \in Tags : TRUE, TRUE>>

Put == \E t \in Tags :
    /\ Len(queue) < MaxSize
    /\ op' = <<"put", t, ~op[3]>>
    /\ queue' = Append(queue, t)

BestMatch(i) == \A j \in DOMAIN queue : LessOrEqual(queue[i], queue[j])

SelectSeqIndex(s, Test(_)) == 
  (*************************************************************************)
  (* The subsequence of s consisting of all elements s[i] such that        *)
  (* Test(i) is true. This is derived from SelectSeq                       *)
  (*************************************************************************)
  LET F[i \in 0..Len(s)] == 
        (*******************************************************************)
        (* F[i] equals SelectSeqIndex(SubSeq(s, 1, i), Test]               *)
        (*******************************************************************)
        IF i = 0 THEN << >>
                 ELSE IF Test(i) THEN Append(F[i-1], s[i])
                                 ELSE F[i-1]
  IN F[Len(s)]

Get ==
    /\ Len(queue) > 0
    /\ \E i \in DOMAIN queue :
        /\ BestMatch(i)
        /\ op' = <<"get", queue[i], ~op[3]>>
        /\ queue' = SelectSeq(queue, LAMBDA j: j # i)

Next == Put \/ Get

Spec == Init /\ [][Next]_vars

=============================================================================
