:- use_module(library(clpfd)).

modulo([], _, []).
modulo([H|T], M, [R|RT]) :-
	R #= H mod M, modulo(T, M, RT).

divide([], _, []).
divide([H|T], D, [R|RT]) :-
	R #= H / D, divide(T, D, RT).

different(W, B) :-
	different_from(W, B).

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
	X #=< Y, sorted([Y|T]).

first([], _).
first([H|_], H).

diagonals([], [], []).
diagonals([H1|T1], [H2|T2], [R1|RT]) :-
    R1 #= H1 - H2,
    diagonals(T1, T2, RT).

diagonals_right([], [], [], _).
diagonals_right([H1|T1], [H2|T2], [R1|RT], N) :-
    R1 #= N - 1 - H1 - H2,
    diagonals_right(T1, T2, RT, N).

ids([], [], [], _).
ids([H1|T1], [H2|T2], [R1|RT], N) :-
    R1 #= N * H1 + H2,
    ids(T1, T2, RT, N).

queens_solve([WR, WC, BR, BC], N) :-
    NP is N-1,

    WR = [WR1, WR2, WR3, WR4, WR5, WR6, WR7, WR8, WR9, WR10],
    WC = [WC1, WC2, WC3, WC4, WC5, WC6, WC7, WC8, WC9, WC10],
    BR = [BR1, BR2, BR3, BR4, BR5, BR6, BR7, BR8, BR9, BR10],
    BC = [BC1, BC2, BC3, BC4, BC5, BC6, BC7, BC8, BC9, BC10],

    V = [WR1, WR2, WR3, WR4, WR5, WR6, WR7, WR8, WR9, WR10,
         WC1, WC2, WC3, WC4, WC5, WC6, WC7, WC8, WC9, WC10,
         BR1, BR2, BR3, BR4, BR5, BR6, BR7, BR8, BR9, BR10,
         BC1, BC2, BC3, BC4, BC5, BC6, BC7, BC8, BC9, BC10],

    diagonals(WR, WC, WDL),
    diagonals(BR, BC, BDL),
    diagonals_right(WR, WC, WDR, N),
    diagonals_right(BR, BC, BDR, N),

    ids(WR, WC, WI, N),
    ids(BR, BC, BI, N),

    sorted(WI), sorted(BI),

    domain(WR, 0, NP),
    domain(WC, 0, NP),
    domain(BR, 0, NP),
    domain(BC, 0, NP),

	different(WC, BC),
	different(WR, BR),

    different(WDL, BDL),
    different(WDR, BDR),

    all_different(WI),
    all_different(BI),

	labeling([], V).

