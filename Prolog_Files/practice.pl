bigger(elephant, mouse).
bigger(elephant, horse).
bigger(horse, mouse).
% factorial of a number
fact(0, 1).
fact(X, N) :- X \= 0, X1 is X - 1, fact(X1, N1), N is X * N1.
% fibonacci of a number
fib(0, 0).
fib(1, 1).
fib(X, N) :- X \= 0, X1 is X - 1, X2 is X - 2, fib(X1, N1), fib(X2, N2), N is N1 + N2.
% is even
isEven(0, 1).
isEven(1, 0).
isEven(X, N) :- X1 is X mod 2, isEven(X1, N1), N is N1.
% length(+L, -N)
listLength([], 0).
listLength([_ | T], N) :- listLength(T, N1), N is N1 + 1.
% prepend to list
listPrepend(L, E, [E | L]).
% append to the list
listAppend([], E, [E]).
listAppend([H | T], E, [H | X]) :- listAppend(T, E, X).
% reverse list
reverseList(L, R) :- reverseList(L, [], R).
reverseList([], A, A).
reverseList([H | T], A, R) :- reverseList(T, [H | A], R).
% check if list has particular element
listHasElem([E | _], E).
listHasElem([H | T], E) :- H \= E, listHasElem(T, E).
% check if binary search tree has particular element
treeHasElem([E, _, _], E).
treeHasElem([H, L, _], E) :- E < H, treeHasElem(L, E).
treeHasElem([H, _, R], E) :- E > H, treeHasElem(R, E).
% insert into binary search tree
treeInsert([], E, [E, [], []]).
treeInsert([E, L, R], E, [E, L, R]).
treeInsert([X, L, R], E, [X, LL, R]) :- E < X, treeInsert(L, E, LL).
treeInsert([X, L, R], E, [X, L, RR]) :- E > X, treeInsert(R, E, RR).
% take one element
addIfNotEqual([], _, []) :- !.
addIfNotEqual([E | T], E, R) :- addIfNotEqual(T, E, R).
% incorrect
%addIfNotEqual([H | T], E, R) :- H \= E, addIfNotEqual(T, E, R1), R is [H | R1].
% correct
addIfNotEqual([H | T], E, [H | R]) :- H \= E, addIfNotEqual(T, E, R).
% myRem[+L1, +L2, -R]
myRem(L, [], L).
myRem(L1, [H2 | T2], R) :- addIfNotEqual(L1, H2, R1), myRem(R1, T2, R).
% List A & B
%addIfEqual([], _, []).
addIfEqual([E | _], E).
addIfEqual([H | T], E) :- H \= E, addIfEqual(T, E).
% myKeep[+L1, +L2, -R]
myKeep([], _, []).
myKeep(_, [], []).
myKeep([H1 | T1], L2, [H1 | R]) :- addIfEqual(L2, H1), myKeep(T1, L2, R).
myKeep([_ | T], L2, R) :- myKeep(T, L2, R).
% reverseList
myReverse(L, R) :- myReverse(L, [], R).
myReverse([], ACC, ACC).
myReverse([H | T], ACC, R) :- myReverse(T, [H | ACC], R).
% check if lists are equal
equalLists([], [], 1).
equalLists([H1 | _], [H2 | _], 0) :- H1 \= H2.
equalLists([H1 | T1], [H2 | T2], R) :- H1 == H2, equalLists(T1, T2, R).
% check if list is symmetrical
symmetricalList([]).
symmetricalList(L, R) :- myReverse(L, R1), equalLists(L, R1, R).
% insert into sorted list
inSortedInsert([], E, [E]).
inSortedInsert([H | T], E, [E | [H | T]]) :- E =< H.
inSortedInsert([H | T], E, [H | R]) :- E > H, inSortedInsert(T, E, R).
% filter odd values
filter_odd_val([], []).
filter_odd_val([H | T], R) :- N1 is H mod 2, N1 == 0, filter_odd_val(T, R).
filter_odd_val([H | T], [H | R]) :- N1 is H mod 2, N1 == 1, filter_odd_val(T, R).
% filter even values
filter_even_val([], []).
filter_even_val([H | T], R) :- N1 is H mod 2, N1 == 1, filter_even_val(T, R).
filter_even_val([H | T], [H | R]) :- N1 is H mod 2, N1 == 0, filter_even_val(T, R).
% append two lists
append([], L, L).
append(L, [], L).
append([H | T], L, [H | R]) :- append(T, L, R).
% sort list to even and odd values
sort_odd_even([], []).
sort_odd_even(L, R) :- filter_odd_val(L, R1), filter_even_val(L, R2), append(R1, R2, R).
% sum numbers in the list
sum([], 0).
sum([H | T], R) :- sum(T, R1), R is H + R1.
% find length of list
lengthList([], 0).
lengthList([_ | T], R) :- lengthList(T, R1), R is R1 + 1.
% find average of all numbers in the list
average(L, R) :- L \= [], lengthList(L, R1), sum(L, R2), R is R2 / R1.
% append two lists
appendTwoLists([], L, L).
appendTwoLists(L, [], L).
appendTwoLists([H | T], L, [H | R]) :- appendTwoLists(T, L, R).
% flatten list
flattenList([], []).
flattenList([[] | T], R) :- flattenList(T, R).
flattenList([[H | T1] | T], R) :- flattenList([H | T1], R1), flattenList(T, R2), appendTwoLists(R1, R2, R).
flattenList([H | T], [H | R]) :- flattenList(T, R).
% find average of list with sublists
average2(L, R) :- flattenList(L, R1), !, average(R1, R).