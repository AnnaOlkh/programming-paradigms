/*
  проста експертна система мовою Prolog
  Варіант 12: об'єкти мають починатися з кириличної літери "С"
*/

:- encoding(utf8).

object('суп').
object('солянка').
object('спагеті').
object('сосиски').
object('салат').
object('суші').
object('сирники').
object('стейк').
object('сендвіч').
object('смузі').
object('самса').
object('сир').

% ----класифікатори------------------

classifier('суп', 'перша страва').
classifier('солянка', 'перша страва').
classifier('спагеті', 'основна страва').
classifier('сосиски', 'мясна страва').
classifier('салат', 'холодна страва').
classifier('суші', 'холодна страва').
classifier('сирники', 'десерт').
classifier('стейк', 'мясна страва').
classifier('сендвіч', 'закуска').
classifier('смузі', 'напій').
classifier('самса', 'випічка').
classifier('сир', 'молочний продукт').

% --ознаки-------------------------

feature('суп', 'температура', 'гаряча').
feature('суп', 'основа', 'овочі').
feature('суп', 'прийом їжі', 'обід').
feature('суп', 'приготування', 'варіння').

feature('солянка', 'температура', 'гаряча').
feature('солянка', 'основа', 'мясо').
feature('солянка', 'прийом їжі', 'обід').
feature('солянка', 'приготування', 'варіння').

feature('спагеті', 'температура', 'гаряча').
feature('спагеті', 'основа', 'макаронні вироби').
feature('спагеті', 'прийом їжі', 'обід').
feature('спагеті', 'приготування', 'варіння').

feature('сосиски', 'температура', 'гаряча').
feature('сосиски', 'основа', 'мясо').
feature('сосиски', 'прийом їжі', 'сніданок').
feature('сосиски', 'приготування', 'варіння').

feature('салат', 'температура', 'кімнатна').
feature('салат', 'основа', 'овочі').
feature('салат', 'прийом їжі', 'обід').
feature('салат', 'прийом їжі', 'вечеря').
feature('салат', 'прийом їжі', 'сніданок').
feature('салат', 'приготування', 'змішування').

feature('суші', 'температура', 'холодна').
feature('суші', 'основа', 'риба').
feature('суші', 'прийом їжі', 'вечеря').
feature('суші', 'приготування', 'складання').

feature('сирники', 'температура', 'гаряча').
feature('сирники', 'основа', 'молочні продукти').
feature('сирники', 'прийом їжі', 'сніданок').
feature('сирники', 'приготування', 'смаження').

feature('стейк', 'температура', 'гаряча').
feature('стейк', 'основа', 'мясо').
feature('стейк', 'прийом їжі', 'вечеря').
feature('стейк', 'прийом їжі', 'обід').
feature('стейк', 'приготування', 'смаження').

feature('сендвіч', 'температура', 'холодна').
feature('сендвіч', 'основа', 'хліб').
feature('сендвіч', 'прийом їжі', 'перекус').
feature('сендвіч', 'прийом їжі', 'сніданок').
feature('сендвіч', 'приготування', 'складання').

feature('смузі', 'температура', 'холодна').
feature('смузі', 'температура', 'кімнатна').
feature('смузі', 'основа', 'фрукти').
feature('смузі', 'прийом їжі', 'перекус').
feature('смузі', 'приготування', 'змішування').

feature('самса', 'температура', 'гаряча').
feature('самса', 'основа', 'тісто').
feature('самса', 'прийом їжі', 'перекус').
feature('самса', 'приготування', 'запікання').

feature('сир', 'температура', 'холодна').
feature('сир', 'основа', 'молочні продукти').
feature('сир', 'прийом їжі', 'перекус').
feature('сир', 'приготування', 'без термічної обробки').

% --Питання---------------------------

question('температура', 'Яка температура страви або продукту?', [
    'гаряча',
    'холодна',
    'кімнатна'
]).

question('основа', 'Який основний інгредієнт?', [
    'овочі',
    'мясо',
    'риба',
    'молочні продукти',
    'хліб',
    'фрукти',
    'макаронні вироби',
    'тісто'
]).

question('прийом їжі', 'Коли цю страву найчастіше вживають?', [
    'сніданок',
    'обід',
    'вечеря',
    'перекус'
]).

question('приготування', 'Який спосіб приготування найближчий?', [
    'варіння',
    'змішування',
    'складання',
    'смаження',
    'запікання',
    'без термічної обробки'
]).

% -----------------------------

start :-
    nl,
    write('Експертна система: визначення страви або продукту на літеру С'), nl,
    write('Відповідайте номером варіанта та ставте крапку після числа.'), nl,
    nl,
    ask_all(Answers),
    best_candidates(Answers, BestScore, Candidates),
    print_result(BestScore, Candidates),
    nl.

ask_all(Answers) :-
    findall(Attribute, question(Attribute, _, _), Attributes),
    ask_attributes(Attributes, Answers).

ask_attributes([], []).
ask_attributes([Attribute | RestAttributes], [Attribute-Value | RestAnswers]) :-
    ask(Attribute, Value),
    ask_attributes(RestAttributes, RestAnswers).

ask(Attribute, Value) :-
    question(Attribute, Text, Options),
    write(Text), nl,
    print_options(Options, 1),
    read(Number),
    (
        select_option(Number, Options, Value)
        ->
        nl
        ;
        write('Некоректний номер. Спробуйте ще раз.'), nl,
        ask(Attribute, Value)
    ).

select_option(Number, Options, Value) :-
    integer(Number),
    length(Options, Count),
    between(1, Count, Number),
    nth1(Number, Options, Value).

print_options([], _).
print_options([Value | Rest], Number) :-
    write(Number), write(' - '), write(Value), nl,
    NextNumber is Number + 1,
    print_options(Rest, NextNumber).


score(Object, Answers, Score) :-
    object(Object),
    findall(1, matches(Object, Answers), Points),
    length(Points, Score).

matches(Object, Answers) :-
    member(Attribute-Value, Answers),
    feature(Object, Attribute, Value).

best_candidates(Answers, BestScore, Candidates) :-
    findall(Score-Object, score(Object, Answers, Score), Pairs),
    max_score(Pairs, BestScore),
    findall(Object, member(BestScore-Object, Pairs), Candidates).

max_score([Score-_ | Rest], BestScore) :-
    max_score(Rest, Score, BestScore).

max_score([], BestScore, BestScore).
max_score([Score-_ | Rest], Current, BestScore) :-
    NewCurrent is max(Score, Current),
    max_score(Rest, NewCurrent, BestScore).

print_result(0, _) :-
    write('Не вдалося визначити обʼєкт за введеними ознаками.').

print_result(Score, [Object]) :-
    Score > 0,
    write('Найімовірніший обʼєкт: '), write(Object), nl,
    write('Класифікатор: '), classifier(Object, Class), write(Class), nl,
    write('Кількість збігів ознак: '), write(Score), nl.

print_result(Score, Objects) :-
    Score > 0,
    Objects = [_, _ | _],
    write('Є кілька можливих варіантів з однаковою кількістю збігів:'), nl,
    print_objects(Objects),
    write('Кількість збігів ознак: '), write(Score), nl.

print_objects([]).
print_objects([Object | Rest]) :-
    write('- '), write(Object), nl,
    print_objects(Rest).
