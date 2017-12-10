/** types A Prolog implementation of a typed language

A Prolog implementation of a simple typed lambda 
calculus extended with records and subtyping rules. 
This languages has the following syntax:

 Term ::= Num
        | Bool 
        | Var 
        | Add Term Term
        | Sub Term Term 
        | And Term Term
        | Or Term Term
        | Not Term                   
        | Let Var Term Term 
        | IfThenElse Term Term Term  -- if then else 
        | \x -> Term                 -- abstraction 
        | Term Term                  -- application 

 Value ::= VInt 
         | VBool 
         | VRecord [Value]
         | \x -> Term 
There are two main predicates, eval and type_check. 
For example: 

==
1 ?- eval(add(num(3), num(4)), X). 
X = num(7) 
==
 
@author rbonifacio
*/


%% eval(Exp, Gamma, X) is det.
%
% An operational semantics of our simple
% typed lambda calculus language + extensions.
% Here we assume that, before evaluating the
% eval predicate, the expressions have been
% previously type checked. 
%

eval(num(N), _, num(N)).

eval(bool(B), _, bool(B)).

eval(add(LHS, RHS), Gamma, num(X)) :-
    eval(LHS, Gamma, num(V1)),
    eval(RHS, Gamma, num(V2)),
    X is V1 + V2. 

eval(sub(LHS, RHS), Gamma, num(X)) :-
    eval(LHS, Gamma, num(V1)),
    eval(RHS, Gamma, num(V2)),
    X is V1 - V2.    

eval(div(LHS, RHS), Gamma, num(X)) :-
    eval(LHS, Gamma, num(V1)),
    eval(RHS, Gamma, num(V2)),
    X is V1 / V2. 

eval(let(Var, Term1, Term2), Gamma, X) :-
    env(Var, Term1, Gamma, NGamma),
    eval(Term2, NGamma, X). 


eval(var(V), Gamma, X) :-
    lookup(V, Gamma, Term),
    eval(Term, Gamma, X).


eval(lambda(arg(X, Type), Term), _, lambda(arg(X, Type), Term)).

eval(app(Term1, Term2), Gamma, X) :-
    eval(Term1, Gamma, lambda(arg(Var, _), Body)),
    eval(Term2, Gamma, Argument),               % inner most strategy!
    env(Var, Argument, Gamma, NGamma),
    eval(Body, NGamma, X).

eval(if0(Cond, Then, _), Gamma, X) :-
    eval(Cond, Gamma, num(0)),
    eval(Then, Gamma, X),
    !.

eval(if0(_,_, Else), Gamma, Y) :-
    eval(Else, Gamma, Y).

eval(if(Cond, Then, _), Gamma, X) :-
    eval(Cond, Gamma, bool(true)),
    eval(Then, Gamma, X),
    !.
eval(if(_, _, Else), Gamma, Y) :-
    eval(Else, Gamma, Y).

eval(and(LHS, RHS), Gamma, bool(true)) :-
    eval(LHS, Gamma, bool(true)),
    eval(RHS, Gamma, bool(true)),
    !.                                          % note the use of the cut operator here!

eval(and(_, _), _, bool(false)).

eval(or(LHS, RHS), Gamma, bool(false)) :-
    eval(LHS, Gamma, bool(false)),
    eval(RHS, Gamma, bool(false)),
    !.                                          % note the use of the cut operator here!

eval(or(_, _), _, bool(true)).


eval(not(V), Gamma, bool(true)) :-
    eval(V, Gamma, bool(false)),
    !.

eval(not(_), _, bool(false)).
 

env(Var, Term, Gamma, [map(Var, Term)|Gamma]). 
    
%% The lookup predicate. 

lookup(Var, [map(Var, Term)|_], Term) :- !. 

lookup(Var, [map(_, _)|Gamma], Term) :- lookup(Var, Gamma, Term).

%% Type checker.

tc(bool(_), _, bool).

tc(num(_),  _, int).

tc(var(V), Gamma, X) :-
    lookup(V, Gamma, X).

tc(add(LHS, RHS), Gamma, int) :-
    tc(LHS, Gamma, int),
    tc(RHS, Gamma, int).

tc(sub(LHS, RHS), Gamma, int) :-
    tc(LHS, Gamma, int),
    tc(RHS, Gamma, int).

tc(div(LHS, RHS), Gamma, int) :-
    tc(LHS, Gamma, int),
    tc(RHS, Gamma, int).

tc(if0(Cond, Then, Else), Gamma, T1) :-
    tc(Cond, Gamma, int),
    tc(Then, Gamma, T1),
    tc(Else, Gamma, T2),
    T1 == T2.

tc(if(Cond, Then, Else), Gamma, T1) :-
    tc(Cond, Gamma, bool),
    tc(Then, Gamma, T1),
    tc(Else, Gamma, T2),
    T1 == T2.

tc(let(Var, Term1, Term2), Gamma, X) :-
    tc(Term1, Gamma, T1),
    tc(Term2,[map(Var, T1)|Gamma], X).

tc(lambda(arg(_, T1), Term), Gamma, arrow(T1, T2)) :-
    tc(Term, Gamma, T2). 

tc(app(E1, E2), Gamma, T2) :-
    tc(E1, Gamma, arrow(T1, T2)),
    tc(E2, Gamma, Ta),
    Ta == T1.

tc(and(LHS, RHS), Gamma, bool) :-
    tc(LHS, Gamma, bool),
    tc(RHS, Gamma, bool). 

tc(or(LHS, RHS), Gamma, bool) :-
    tc(LHS, Gamma, bool),
    tc(RHS, Gamma, bool). 

tc(not(V), Gamma, bool) :-
    tc(V, Gamma, bool).


%% eval(let(inc, lambda(arg(x,int), add(var(x), num(1))), app(var(inc),num(5))), [], X).
