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

eval(lambda(arg(X, Type), Term), _, lambda(arg(X, Type), Term)).

eval(add(LHS, RHS), Gamma, num(X)) :-
    eval(LHS, Gamma, num(V1)),
    eval(RHS, Gamma, num(V2)),
    X is V1 + V2. 

eval(and(LHS, RHS), Gamma, bool(true)) :-
    eval(LHS, Gamma, bool(true)),
    eval(RHS, Gamma, bool(true)),
    !.                                          % note the use of the cut operator here!

eval(and(_, _), _, bool(false)). 

eval(let(Var, Term1, Term2), Gamma, X) :-
    env(Var, Term1, Gamma, NGamma),
    eval(Term2, NGamma, X). 

eval(var(V), Gamma, X) :-
    lookup(V, Gamma, Term),
    eval(Term, Gamma, X). 
    
eval(app(Term1, Term2), Gamma, X) :-
    eval(Term1, Gamma, lambda(arg(Var, _), Body)),
    eval(Term2, Gamma, Argument),               % inner most strategy!
    env(Var, Argument, Gamma, NGamma),
    eval(Body, NGamma, X).

env(Var, Term, Gamma, [map(Var, Term)|Gamma]). 
    
%% The lookup predicate. 

lookup(Var, [map(Var, Term)|_], Term) :- !. 

lookup(Var, [map(_, _)|Gamma], Term) :- lookup(Var, Gamma, Term).

%% Type checker.

tc(num(_),  _, int).

tc(bool(_), _, bool).

tc(lambda(arg(_, T1), Term), Gamma, arrow(T1, T2)) :-
    tc(Term, Gamma, T2). 

tc(add(LHS, RHS), Gamma, int) :-
    tc(LHS, Gamma, int),
    tc(RHS, Gamma, int).

tc(and(LHS, RHS), Gamma, bool) :-
    tc(LHS, Gamma, bool),
    tc(RHS, Gamma, bool). 

% eval(let(inc, lambda(arg(x,int), add(var(x), num(1))), app(var(inc),num(5))), [], X).