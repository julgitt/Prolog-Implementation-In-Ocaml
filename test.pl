mother(ania, basia).
mother(basia, piotr).
father(olek, ania).
father(olek, kacper).
parent(X, Y) :- mother(X, Y).
parent(X, Y) :- father(X, Y).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

/*zero(N) :- N is 0.
nat(X) :- zero(X).
nat(N) :- M is N - 1, nat(M).
increase(S, T) :- T is S + 1. 
length([], 0).
length([_|L], N) :- length(L, M), N is M + 1.*/
