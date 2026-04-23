/* Розбити список на два списки відповідно до умови: "бути чи не бути факторіалом деякого числа" */

split_factorials([], [], []).

split_factorials([X|Xs], Factorials, Others) :-
    ( is_factorial(X, 2) ->
        Factorials = [X|Fs],
        Others = Os
    ;
        Factorials = Fs,
        Others = [X|Os]
    ),
    split_factorials(Xs, Fs, Os).

is_factorial(1, _).
is_factorial(N, K) :-
    N > 1,
    N >= K,
    0 is N mod K,
    N1 is N // K,
    K1 is K + 1,
    is_factorial(N1, K1).

/*
?- split_factorials([1, 2, 3, 4, 5, 6, 24, 25], F, O).
F = [1, 2, 6, 24],
O = [3, 4, 5, 25].

?- split_factorials([2, 6, 24, 120], F, O).
F = [2, 6, 24, 120],
O = [].

?- split_factorials([-5, 0, 4, 10], F, O).
F = [],
O = [-5, 0, 4, 10].

?- split_factorials([], F, O).
F = [],
O = [].*/