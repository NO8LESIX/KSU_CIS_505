/*Code to verify if a list contains all zeros*/
allzero([]).
allzero([H|T]) :-
	H = 0 -> allzero(T).
/* Code for the Summation of a tree's nodes*/
leaf(V).
node(L, R).

sum(leaf(N), V):-
	V = N.
sum(node(L, R), V):-
	sum(L, V1), sum(R, V2),
	V is V1 + V2.
	
/*Code for Sublist*/
sublist([],_).
sublist([H|T], [H|Ts]) :- sublist(T, Ts).
sublist([H|T], [_|Ts]) :- sublist([H|T], Ts).
	


/*

sum(node(_,_), V):-
	V is 0 .
sum(node(_,R), V):-
	sum(R, V1),
	V = V1.
sum(node(L,_), V):-
	sum(L, V1),
	V = V1.

sum(leaf(N), V):-
	V = N .
sum(node(_,_), V):-
	V is V + 0 .
sum(node(_,R), V):-	
	V is sum(R, V).
sum(node(L,_), V):-
	V is sum(L, V).
sum(node(L, R), V):-
	V is sum(L, V) + sum(R, V).
	*/