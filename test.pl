mother(ania, basia).
mother(basia, piotr).
father(olek, ania).
father(olek, kacper).
parent(X, Y) :- mother(X, Y).
parent(X, Y) :- father(X, Y).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

/*length(nil, 0).
length(cons(_, L), N) :- length(L, M), N is M + 1.*/

length([], 0).
length([_ | L], N) :- length(L, M), N is M + 1.

list_member(X, [X | _]).
list_member(X, [_ | TAIL]) :- list_member(X, TAIL).

list_concat([], L, L).

list_concat([X1 | L1], L2, [X1 | L3]) :- my_list_concat(L1, L2, L3).

list_delete(X, [X], []).
list_delete(X, [X | L1], L1).
list_delete(X, [Y | L2], [Y | L1]) :- list_delete(X, L2, L1).