% insert one element into sorted array
insertIntoSorted([], E, [E]).
insertIntoSorted([H | T], E, [E | [H | T]]) :- E =< H.
insertIntoSorted([H | T], E, [H | R]) :- E > H, insertIntoSorted(T, E, R).
% length of the list
myLength([], 0).
myLength([_ | T], R) :- myLength(T, R1), R is R1 + 1.
% prepend to list
myPrepend([], E, [E]).
myPrepend([H | T], E, [E | [H | T]]).
% append to the list
myAppend([], E, [E]).
myAppend([H | T], E, [H | R]) :- myAppend(T, E, R).
% take reverse of the list
myReverse(L, R) :- myReverse(L, [], R).
myReverse([], A, A).
myReverse([H | T], A, R) :- myReverse(T, [H | A], R).
% reverse of the list
myReverse1(L, R) :- myReverse1(L, [], R).
myReverse1([], A, A).
myReverse1([H | T], A, R) :- myReverse1(T, [H | A], R).
% append two lists
appendLists([], L, L).
appendLists(L, [], L).
appendLists([H | T], L, [H | R]) :- appendLists(T, L, R).
% flatten list
flattenList([], []).
flattenList([[] | T], R) :- flattenList(T, R).
flattenList([[H | T1] | T], R) :- flattenList([H | T1], R1), flattenList(T, R2), appendLists(R1, R2, R).
flattenList([H | T], [H | R]) :- flattenList(T, R).
% flatten list
flattenList1([], []).
flattenList1([[] | T], R) :- flattenList1(T, R).
flattenList1([[H | T1] | T], R) :- flattenList1([H | T1], R1), flattenList1(T, R2), appendLists(R1, R2, R).
flattenList1([H | T], [H | R]) :- flattenList1(T, R).
% check if list has element
myHasElem([E | _], E).
myHasElem([H | T], E) :- H \= E, myHasElem(T, E).
% check if tree has element
myTreeHas([E, _, _], E).
myTreeHas([X, L, _], E) :- E < X, myTreeHas(L, E).
myTreeHas([X, _, R], E) :- E > X, myTreeHas(R, E).
% insert into BST
insertBST([], E, [E, [], []]).
insertBST([E, L, R], E, [E, L, R]).
insertBST([X, L, R], E, [X, LL, R]) :- E < X, insertBST(L, E, LL).
insertBST([X, L, R], E, [X, L, RR]) :- E > X, insertBST(R, E, RR).
% insert sort
insertSort(L, R) :- insertSort(L, [], R).
insertSort([], A, A).
insertSort([H | T], A, R) :- insertIntoSorted(A, H, R1), insertSort(T, R1, R).
% find reverse of list
reverseList(L, R) :- reverseList(L, [], R).
reverseList([], A, A).
reverseList([H | T], A, R) :- reverseList(T, [H | A], R).
% find if two lists are equal
equalLists([], [], 1).
equalLists([H1 | T1], [H2 | T2], R) :- H1 == H2, equalLists(T1, T2, R).
% find if symmetrical
symmetricalList([]).
symmetricalList(L) :- reverseList(L, R1), equalLists(L, R1, R), R == 1.
% filter odd values
filter_odd_val([], []).
filter_odd_val([H | T], R) :- N is H mod 2, N == 0, filter_odd_val(T, R).
filter_odd_val([H | T], [H | R]) :- N is H mod 2, N == 1, filter_odd_val(T, R).
% filter even values
filter_even_val([], []).
filter_even_val([H | T], R) :- N is H mod 2, N == 1, filter_even_val(T, R).
filter_even_val([H | T], [H | R]) :- N is H mod 2, N == 0, filter_even_val(T, R).
% append two lists
appendLists1([], L, L).
appendLists1(L, [], L).
appendLists1([H | T], L, [H | R]) :- appendLists1(T, L, R).
% sort odd then even
sort_odd_even_val([], []).
sort_odd_even_val(L, R) :- filter_odd_val(L, R1), filter_even_val(L, R2), appendLists1(R1, R2, R).
% find sum of list
sumList([], 0).
sumList([H | T], R) :- sumList(T, R1), R is R1 + H.
% find list length
lengthList([], 0).
lengthList([_ | T], R) :- lengthList(T, R1), R is R1 + 1.
% find average
avg(L, R) :- L \= [], sumList(L, R1), lengthList(L, R2), R is R1 / R2.
% append two lists
appendLists2([], L, L).
appendLists2(L, [], L).
appendLists2([H | T], L, [H | R]) :- appendLists2(T, L, R).
% flatten list
flattenList2([], []).
flattenList2([[] | T], R) :- flattenList2(T, R).
flattenList2([[H | T1] | T], R) :- flattenList2([H | T1], R1), flattenList2(T, R2), appendLists2(R1, R2, R).
flattenList2([H | T], [H | R]) :- flattenList2(T, R).
% average of list with sublists
avg2(L, R) :- flattenList2(L, R1), !, avg(R1, R).