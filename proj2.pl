% ==============================================================================
% Declarative Programming Project 2
% This module processes the crossword puzzle game described in the specification
% TODO:   asdfhohfoihfiw
% @author Qini ZHANG(qiniz)
% ==============================================================================

%   Library import:
:- ensure_loaded(library(clpfd)).

%   The main predicate of the program
%   PuzzleFile:     The puzzle file to read in
%   WordlistFile:   The list of words used to filled in puzzle
%   SolutionFile:   The file contains the result of soving puzzle
main(PuzzleFile, WordlistFile, SolutionFile) :-
    %   Read in puzzle and wordlist,
    %   replace the underscore into a specific letter/number
    read_file(PuzzleFile, Puzzle),
    read_file(WordlistFile, Wordlist),

    %   Check if the puzzle is valid
    valid_puzzle(Puzzle),

    %   Solve the puzzle and print it out
    solve_puzzle(Puzzle, Wordlist),
    print_puzzle(SolutionFile, Puzzle).

%   Read the given file
%   Filename:   The name of file to read
%   Content:    Store the content
read_file(Filename, Content) :-
    open(Filename, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

%   Read all lines in given stream
%   Stream:     The stream to read from
%   Content:    store the content
read_lines(Stream, Content) :-
    read_line(Stream, Line, Last),
    (   Last = true
    ->  (   Line = []
        ->  Content = []
        ;   Content = [Line]
        )
    ;  Content = [Line|Content1],
        read_lines(Stream, Content1)
    ).

%   Read a line of given stream
%   Stream:     The stream to read from
%   Line:       The line aims to store letter/number
%   Last:       A flag to check if it is the last line
read_line(Stream, Line, Last) :-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Line = [],
        Last = true
    ; Char = '\n'
    ->  Line = [],
        Last = false
    ; Char = '_'
    ->  Line = [_NewVariable|Line1],
        read_line(Stream, Line1, Last)
    ; Line = [Char|Line1],
        read_line(Stream, Line1, Last)
    ).

%   Print the result to given SolutionFile
%   SolutionFile:   The file to store the result
%   Puzzle:         the puzzle to be print
print_puzzle(SolutionFile, Puzzle) :-
    open(SolutionFile, write, Stream),
    maplist(print_row(Stream), Puzzle),
    close(Stream).

%   Print the given row to the stream
%   Stream: The stream to be printed
%   Row:    The row to print
print_row(Stream, Row) :-
    maplist(put_puzzle_char(Stream), Row),
    nl(Stream).

%   Print the character into right stream
%   Stream: The stream to print to
%   Char:   The character to print
put_puzzle_char(Stream, Char) :-
    (   var(Char)
    ->  put_char(Stream, '_')
    ;   put_char(Stream, Char)
    ).

%   Check if the puzzle is valid
%   Take in the Puzzle to check
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
    maplist(samelength(Row), Rows).

%   Check if the length of two list is same
samelength([], []).
samelength([_|L1], [_|L2]) :-
    same_length(L1, L2).
%===============================================================================
solve_puzzle(Puzzle, WordList) :-
    %   make tranpose versioon of the horizontal puzzle
    transpose(Puzzle, VerPuzzle),

    %   make a list of slots
    make_slots(Puzzle, [], Slots1),
    make_slots(VerPuzzle, Slots1, Slots),

    %   Put the words into Slots1
    put_into_slots(Slots, WordList).

%   make slots and transfer a puzzle into a list of slots, then we can put it in
%   make_slots(HorPuzzle, SlotsInput, SlotsOutput)
%   Puzzle:         the puzzle to be converted into slots
%   SlotsInput:     the main list of slots append to
%   SlotsOutput:    the list of slots in Puzzle aim to append in SlotsInput.
make_slots([], Slots0, Slots0).
make_slots([Row|Rows], Slots0, Slots) :-
    change_row(Row, [], Slots0, Slots1),
    make_slots(Rows, Slots1, Slots).

%   change_row(PuzzleRow, Current, SlotsInput, SlotsOutput)
%   PuzzleRow:  a puzzle row ready to process
%   Current:    the current word ready to be make slots
%   SlotsInput: the main list of slots append to
%   SlotsOutput:the list of slots in puzzle aim to append in SlotsInput
change_row([], Current, Slots0, Slots) :-
    (   Current == []
    ->  unify(Slots, Slots0)
    ;   (   length(Current, LengthCurrent), LengthCurrent > 1
        ->    append(Slots0, [Current], Slots)
        ;   unify(Slots, Slots0)
        )
    ).
change_row([Char|Tail], Current, Slots0, Slots) :-
    (   Char == '#'
    ->  (   Current == []
        ->  change_row(Tail, [], Slots0, Slots)
        ;   (   length(Current, LengthCurrent), LengthCurrent > 1
            ->  append(Slots0, [Current], Slots1),
                change_row(Tail, [], Slots1, Slots)
            ;   change_row(Tail, [], Slots0, Slots)
            )
        )
    ;   append(Current, [Char], NewCurrent),
        change_row(Tail, NewCurrent, Slots0, Slots)
    ).

%   put_into_slots(Slots, WordList)
put_into_slots([], _).
put_into_slots([Slot|Slots], WordList) :-
    %   find how many possibility of the given slot matches
    get_matches(Slot, WordList, 0, Matches),

    %   find fewest mathcing words
    fewest_matches(Slots, WordList, Matches, Slot, TargetSlot, [], NewSlots),

    %   put into a slot
    put_into_a_slot(TargetSlot, WordList, [], NewWordList),

    %   Do same things for the rest slots
    put_into_slots(NewSlots, NewWordList).


%   Based on hint 5, return the number of possibilities for the given slot
%   get_matches(Slot, WordList, MatchesCurrent, MatchesTotal).
get_matches(_, [], MatchesCurrent, MatchesCurrent).
get_matches(Slot, [W|Ws], MatchesCurrent, MatchesTotal) :-
    (   can_be_unified(Slot, W)
    ->  get_matches(Slot, Ws, MatchesCurrent + 1, MatchesTotal)
    ;   get_matches(Slot, Ws, MatchesCurrent, MatchesTotal)
    ).

%   Based on hint 6, count the number of words that match each slot,
%   and select (one of) the slot(s) with the fewest matching words to fill.
%   fewest_matches(Slots, WordList, FewestMatches, CurrentSlot,
%                  TargetSlot, NewSlots0, NewSlots)
fewest_matches([], _, FewestMatches, CurrentSlot, CurrentSlot, NewSlots0,
                NewSlots0) :-
    %   no need to fill since there is nothing input
    FewestMatches > 0.
fewest_matches([Slot|Slots], WordList, FewestMatches, CurrentSlot, TargetSlot,
                NewSlots0, NewSlots) :-
    %   find how many possible matches there are for the given slot
    get_matches(Slot, WordList, 0, MatchesTotal),
    (   MatchesTotal < FewestMatches
    ->  %   This is the new TargetSlot, then update
        append(NewSlots0, [CurrentSlot], NewSlots1),
        fewest_matches(Slots, WordList, MatchesTotal, Slot, TargetSlot,
                        NewSlots1, NewSlots)

    ;   %   if not satisfied the condition, it means the old one is better
        append(NewSlots0, [Slot], NewSlots1),
        fewest_matches(Slots, WordList, FewestMatches, CurrentSlot, TargetSlot,
                        NewSlots1, NewSlots)
    ).

%   Check if the two words can be unified
%   can_be_unified(Word1, Word2)
can_be_unified([], []).
can_be_unified([W1|Ws1], [W2|Ws2]) :-
    (   (W1 == W2; var(W1); var(W2))
    ->  can_be_unified(Ws1, Ws2)
    ).

%   Unifies two word lists
unify(Word1, Word1).

%   put_into_a_slot(Slot, WordList, NewWordList0, NewWordList)
put_into_a_slot(Slot, [W|Ws], NewWordList0, NewWordList) :-
    %   try to put into the slot
    unify(Slot, W),
    append(Ws, NewWordList0, NewWordList);

    %   if cannot put it in, just ignore it and try next one
    append(NewWordList0, [W], NewWordList1),
    put_into_a_slot(Slot, Ws, NewWordList1, NewWordList).
%===============================================================================


















