%Для заданого натурального k виявити, чи допускає скінчений автомат хоча б одне слово
%довжини k. При ствердній відповіді навести приклад відповідного слова.

start(contains_ab, q0).

final(contains_ab, q2).

transition(contains_ab, q0, a, q1).
transition(contains_ab, q0, b, q0).
transition(contains_ab, q1, a, q1).
transition(contains_ab, q1, b, q2).
transition(contains_ab, q2, a, q2).
transition(contains_ab, q2, b, q2).


start(epsilon_accept, q0).
final(epsilon_accept, q0).
transition(epsilon_accept, q0, a, q0).


start(no_empty, q0).
final(no_empty, q1).
transition(no_empty, q0, a, q1).


start(dead_end, q0).
final(dead_end, q2).
transition(dead_end, q0, a, q1).


start(loop_accept, q0).
final(loop_accept, q1).
transition(loop_accept, q0, a, q1).
transition(loop_accept, q1, a, q1).

start(nondet, q0).
final(nondet, q2).
transition(nondet, q0, a, q1).
transition(nondet, q0, a, q2).


accepted_word(Automaton, K, Word) :-
    start(Automaton, StartState),
    path(Automaton, StartState, K, Word, EndState),
    final(Automaton, EndState).


path(_, State, 0, [], State).

path(Automaton, CurrentState, K, [Symbol | RestWord], EndState) :-
    K > 0,
    transition(Automaton, CurrentState, Symbol, NextState),
    K1 is K - 1,
    path(Automaton, NextState, K1, RestWord, EndState).