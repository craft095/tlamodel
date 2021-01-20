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

Min(S) ==
    CHOOSE x \in S:
        \A y \in S: y >= x

BestMatch ==
    LET highest == {
        i \in DOMAIN queue :
            \A j \in DOMAIN queue : LessOrEqual(queue[i], queue[j]) }
    IN Min(highest)

SelectSeqIndex(s, Test(_)) ==
  (*************************************************************************)
  (* The subsequence of s consisting of all elements s[i] such that        *)
  (* Test(i) is true. This is derived from SelectSeq                       *)
  (*************************************************************************)
  LET F[k \in 0..Len(s)] ==
        (*******************************************************************)
        (* F[i] equals SelectSeqIndex(SubSeq(s, 1, i), Test]               *)
        (*******************************************************************)
        IF k = 0 THEN << >>
                 ELSE IF Test(k) THEN Append(F[k-1], s[k])
                                 ELSE F[k-1]
  IN F[Len(s)]

Get ==
    /\ Len(queue) > 0
    /\ \E i \in DOMAIN queue :
        /\ i = BestMatch
        /\ op' = <<"get", queue[i], ~op[3]>>
        /\ queue' = SelectSeqIndex(queue, LAMBDA j: j /= i)

Next == Put \/ Get

Spec == Init /\ [][Next]_vars

=============================================================================
