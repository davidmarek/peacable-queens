:- use_module(library(clpfd)).

modulo([], _, []).
modulo([H|T], M, [R|RT]) :-
	R #= H mod M, modulo(T, M, RT).

divide([], _, []).
divide([H|T], D, [R|RT]) :-
	R #= H / D, divide(T, D, RT).

different(W, B) :-
	different_from(W, B), different_from(B, W).

not_member(_, []).
not_member(X, [H|T]) :-
	X #\= H, not_member(X, T).

different_from([], _).
different_from([H|T], O) :-
	not_member(H, O), different_from(T, O).

subtract([], _, R, R).
subtract([H|T], S, O, [R|RT]) :-
	R #= H - S, subtract(T, S, O, RT).

subtract_pairwise(X, Y, R) :-
	subtract_pairwise_help(X, Y, [], R).
subtract_pairwise_help(_, [], R, R).
subtract_pairwise_help([], _, R, R).
subtract_pairwise_help(X, [H|T], O, R) :-
	subtract(X, H, O, R1), subtract_pairwise_help(X, T, R1, R).

sorted([]).
sorted([_]).
sorted([X,Y|T]) :-
	X #< Y, sorted([Y|T]).

first([], _).
first([H|_], H).

queens_solve(V, N, C) :-
	NP is N * N - 1,
	LL is N-1,
	RL is N+1,

	length(W, C),
	length(B, C),

	sorted(W),
	sorted(B),

	append(W, B, V),

	domain(W, 0, NP),
	domain(B, 0, NP),

	modulo(W, N, WC),
	modulo(B, N, BC),
	divide(W, N, WR),
	divide(B, N, BR),

	different(WC, BC),
	different(WR, BR),

	subtract_pairwise(W, B, D),
	modulo(D, LL, LD),
	modulo(D, RL, RD),
	not_member(0, LD),
	not_member(0, RD),

	all_different(V),
	labeling([], V).

