/* Залишити у першому списку елементи, що входять у другий список лише один раз */

:- use_module(library(aggregate)).

only_elements([], _, []).
only_elements([X|Xs], Ys, [X|Result]) :-
    aggregate_all(count, member(X, Ys), 1),
    only_elements(Xs, Ys, Result).
only_elements([X|Xs], Ys, Result) :-
    aggregate_all(count, member(X, Ys), N),
    N =\= 1,
    only_elements(Xs, Ys, Result).

/*
?- only_elements([1,2,3,4], [1,1,2,4,5], R).
R = [2,4].

?- only_elements([10,20,30], [30,20,10,40], R).
R = [10,20,30].

?- only_elements([5,6,7], [5,5,8,9], R).
R = [].

?- only_elements([1,2,3], [], R).
R = [].
*/