:- use_module(library(clpfd)).
:- use_module(library(lists)).

% 0 = Nothing
% 1 = Black
% 2 = White
diff_from_list(_, []).
diff_from_list(X, [H|T]) :-
    X + H #\= 3,
    diff_from_list(X, T).

diff_on_row([]).
diff_on_row([H|T]) :-
    diff_from_list(H, T),
    diff_on_row(T).

diff_rows([]).
diff_rows([H|T]) :-
    diff_on_row(H),
    diff_rows(T).

flatten([], []).
flatten([H|T], R) :- append(H, R1, R), flatten(T, R1).

exactly(_, [], 0).
exactly(X, [Y|L], N) :-
    X #= Y #<=> B,
    N #= M+B,
    exactly(X, L, M).

diag([], _, []).
diag([X|_], 0, [Y]) :-
    nth0(0, X, Y).
diag([X|Xs], I, [H|T]) :- 
    nth0(I, X, H), I1 is I - 1, diag(Xs, I1, T).
diag([_|Xs], I, R) :-
    I1 is I - 1, diag(Xs, I1, R).

diags(_, 0, []).
diags(X, I, [H|T]) :-
    diag(X, I, H),
    I1 is I - 1,
    diags(X, I1, T).

reversed_diags(X, I, D) :-
    reverse_rows(X, XR),
    diags(XR, I, D).

reverse_rows([], []).
reverse_rows([X|Xs], [R|Rs]) :- reverse(X, R), reverse_rows(Xs, Rs).

all_diags(X, I, D) :-
    diags(X, I, D1),
    reversed_diags(X, I, D2),
    append(D1, D2, D).

create_rows_help(_, 0, []).
create_rows_help(N, L, [X1|X2]) :-
    length(X1, N),
    L1 is L - 1,
    create_rows_help(N, L1, X2).

create_rows(N, X) :-
    create_rows_help(N, N, X).

queens(NQ, Rows) :-
    Rows = [
        [R11, R12, R13, R14, R15, R16, R17, R18, R19],
        [R21, R22, R23, R24, R25, R26, R27, R28, R29],
        [R31, R32, R33, R34, R35, R36, R37, R38, R39],
        [R41, R42, R43, R44, R45, R46, R47, R48, R49],
        [R51, R52, R53, R54, R55, R56, R57, R58, R59],
        [R61, R62, R63, R64, R65, R66, R67, R68, R69],
        [R71, R72, R73, R74, R75, R76, R77, R78, R79],
        [R81, R82, R83, R84, R85, R86, R87, R88, R89],
        [R91, R92, R93, R94, R95, R96, R97, R98, R99]
    ],
    all_diags(Rows, 15, Diags),
    transpose(Rows, Cols),

    diff_rows(Rows),
    diff_rows(Cols),
    diff_rows(Diags),

    flatten(Rows, Vars),

    exactly(1, Vars, NQ),
    exactly(2, Vars, NQ),

    domain(Vars, 0, 2),
    labeling([], Vars).


