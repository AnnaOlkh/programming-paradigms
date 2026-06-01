/*Реалізація перевірки КВ-граматики бути граматикою Кореняка-Хопкрофта
(у граматиці Кореняка-Хопкрофта для кожного з нетерміналів
правила мають починатися з різних термінальних символів.)*/

start(valid, s).

nonterminal(valid, s).
nonterminal(valid, a_nt).

terminal(valid, a).
terminal(valid, b).
terminal(valid, c).
terminal(valid, d).

rule(valid, s, [a, a_nt]).
rule(valid, s, [b]).
rule(valid, a_nt, [c, s]).
rule(valid, a_nt, [d]).

%--------------------------
start(invalid, a).

nonterminal(invalid, s).
nonterminal(invalid, a_nt).
nonterminal(invalid, b_nt).

terminal(invalid, a).
terminal(invalid, b).
terminal(invalid, c).
terminal(invalid, d).

rule(invalid, s, [a, a_nt]).
rule(invalid, s, [a, b_nt]).
rule(invalid, a_nt, [s, a]).
rule(invalid, b_nt, []).
rule(invalid, a, [b]).
rule(invalid, b_nt, [c, unknown_symbol]).

%----------------------------------
declared_symbol(G, X) :-
    terminal(G, X).

declared_symbol(G, X) :-
    nonterminal(G, X).

all_symbols_declared(_, []).

all_symbols_declared(G, [Symbol | Rest]) :-
    declared_symbol(G, Symbol),
    all_symbols_declared(G, Rest).

first_symbol(G, N, First) :-
    rule(G, N, [First | _]).

first_terminal(G, N, First) :-
    first_symbol(G, N, First),
    terminal(G, First).

has_duplicate([Head | Tail], Head) :-
    member(Head, Tail).

has_duplicate([_ | Tail], Duplicate) :-
    has_duplicate(Tail, Duplicate).

% --------------------------------------------------------------

grammar_exception(G, start_symbol_is_not_nonterminal(Start)) :-
    start(G, Start),
    \+ nonterminal(G, Start).

grammar_exception(G, left_part_is_not_nonterminal(N)) :-
    rule(G, N, _),
    \+ nonterminal(G, N).

grammar_exception(G, empty_rule(N)) :-
    rule(G, N, []).

grammar_exception(G, first_symbol_is_not_terminal(N, First)) :-
    rule(G, N, [First | _]),
    \+ terminal(G, First).

grammar_exception(G, unknown_symbol_in_rule(N, Right)) :-
    rule(G, N, Right),
    \+ all_symbols_declared(G, Right).

grammar_exception(G, same_first_terminal(N, Duplicate)) :-
    nonterminal(G, N),
    findall(First, first_terminal(G, N, First), FirstSymbols),
    has_duplicate(FirstSymbols, Duplicate).

%-----------------------------------
is_kh_grammar(G) :-
    \+ grammar_exception(G, _).

check(G) :-
    is_kh_grammar(G).


show_exceptions(G) :-
    grammar_exception(G, Exception),
    writeln(Exception),
    fail.

show_exceptions(_).