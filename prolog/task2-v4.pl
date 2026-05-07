lower_peaks(List, (Peaks, Count)) :-
    find_peaks(List, Peaks),
    length(Peaks, Count).

find_peaks([], []).
find_peaks([_], []).

find_peaks([First, Second | Rest], Peaks) :-
    (
        First < Second ->
            Peaks = [(First, 1) | OtherPeaks]
        ;
            Peaks = OtherPeaks
    ),
    find_peaks_middle(First, Second, Rest, 2, OtherPeaks).

find_peaks_middle(Left, Current, [], Pos, Peaks) :-
    (
        Current < Left ->
            Peaks = [(Current, Pos)]
        ;
            Peaks = []
    ).

find_peaks_middle(Left, Current, [Right | Rest], Pos, Peaks) :-
    NextPos is Pos + 1,
    (
        Current < Left,
        Current < Right ->
            Peaks = [(Current, Pos) | OtherPeaks]
        ;
            Peaks = OtherPeaks
    ),
    find_peaks_middle(Current, Right, Rest, NextPos, OtherPeaks).

run_tests :-
    writeln('--- Test 1 ---'),
    writeln('Input: [5, 4, 2, 8, 3, 1, 6, 9, 5]'),
    writeln('Expected: ([(2,3),(1,6),(5,9)],3)'),
    writeln('Result:'),
    lower_peaks([5, 4, 2, 8, 3, 1, 6, 9, 5], Result1),
    writeln(Result1),

    nl,
    writeln('--- Test 2 ---'),
    writeln('Input: [10, 8, 6, 4]'),
    writeln('Expected: ([(4,4)],1)'),
    writeln('Result:'),
    lower_peaks([10, 8, 6, 4], Result2),
    writeln(Result2),

    nl,
    writeln('--- Test 3 ---'),
    writeln('Input: [2, 5, 8, 12]'),
    writeln('Expected: ([(2,1)],1)'),
    writeln('Result:'),
    lower_peaks([2, 5, 8, 12], Result3),
    writeln(Result3),

    nl,
    writeln('--- Test 4 ---'),
    writeln('Input: []'),
    writeln('Expected: ([],0)'),
    writeln('Result:'),
    lower_peaks([], Result4),
    writeln(Result4),

    nl,
    writeln('--- Test 5 ---'),
    writeln('Input: [1]'),
    writeln('Expected: ([],0)'),
    writeln('Result:'),
    lower_peaks([1], Result5),
    writeln(Result5),

    nl,
    writeln('--- Test 6 ---'),
    writeln('Input: [7, 3]'),
    writeln('Expected: ([(3,2)],1)'),
    writeln('Result:'),
    lower_peaks([7, 3], Result6),
    writeln(Result6),

    nl,
    writeln('--- Test 7 ---'),
    writeln('Input: [3, 8, 10, 6, 2]'),
    writeln('Expected: ([(3,1),(2,5)],2)'),
    writeln('Result:'),
    lower_peaks([3, 8, 10, 6, 2], Result7),
    writeln(Result7).