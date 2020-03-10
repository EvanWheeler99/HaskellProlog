/*
CPSC 449 Assignment 4
By: Evan Wheeler
ID# 30046173
*/

%Question 1
%Knowledge Base
mother(c, a).
mother(c, b).
mother(g, c).
mother(g, d).
mother(d, f).
%----------------

%X is the sister of Y
sister(X, Y) :- mother(Z, X), mother(Z, Y), X \== Y.

%X is a cousin of Y
cousin(X, Y) :- mother(M, X), mother(N, Y), sister(M, N), X \== Y.

%X is a granddaughter of Y
granddaughter(X, Y) :- mother(Y, N), mother(N, X).

/*I assume descendant means a direct descendant of the person, meaning counsins and sisters are not descendants
but a grand-child is a descendent of their grandmother, and children are the descendents of their mothers.
X is a descendent of Y                                                                                  */
descendent(X, Y) :- mother(Y, X).
descendent(X, Y)  :- descendent(Z, Y), mother(Z, X).

%Question 2
%member_new(X, Y) iff x is a member of Y
member_new(X, [X | _]).
member_new(X, [_ | N]) :- member_new(X, N).

%Question 3
%Subset(X, Y) iff X is a subset of Y
subset([] ,_).
subset([X | XS], Y) :- member_new(X, Y), subset(XS, Y).

%Quesstion 4
%disjoint(X, Y) iff X and Y are disjoint
disjoint([] ,_).
disjoint([X | XS], Y) :- \+ ( member_new(X, Y)), disjoint(XS, Y).

%Question 5
%union(X, Y, N) iff N is (X U Y)
union([], X, X).
union([X | XS], Y, N) :- member(X, Y), union(XS, Y, N).
union([X | XS], Y, [X | NS]) :- \+ ( member(X, Y)), union(XS, Y, NS).

%Question 6
%intersection(X, Y, N) iff N is (X intersection Y)
intersection([], _, []).
intersection([X | XS], Y, [X | NS]) :- member(X, Y), intersection(XS, Y, NS).
intersection([X | XS], Y, N) :- \+ ( member(X, Y)), intersection(XS, Y, N).

%Question 7
%difference(X, Y, N) iff N is L - K
difference([], _, []).
difference([X | XS], Y, N) :- member(X, Y), difference(XS, Y, N).
difference([X | XS], Y, [X | NS]) :- \+ (member(X, Y)), difference(XS, K, NS).

%Quetstion 8
%Occurrences(X, Y, N) iff X appears N times in Y
occurrences(_, [], 0).
occurrences(X, [Y | YS], N) :- \+ (X = Y), occurrences(X, YS, N).
occurrences(X, [X | YS], N) :-  occurrences(X, YS, M), N is (M+1).

%Question 9
%quicksort(X, Y) Y is X quicksorted into ascending order
quicksort([], []).
quicksort([X|XS], Y) :- pivot(X, XS, A, B), quicksort(A, SortedA), quicksort(B, SortedB), append(SortedA, [X|SortedB], Y), !.

pivot(_, [], [], []).
pivot(P, [X | XS], [X | N], M) :- P >= X, pivot(P, XS, N, M).
pivot(P, [X | XS], N, [X | M]) :- pivot(P, XS, N, M).

%Question 10
%Knowledge Base
edge(1,2). % read as there is an edge from 1 to 2
edge(1,4).
edge(1,3).
edge(2,3).
edge(2,5).
edge(3,4).
edge(3,5).
edge(4,5).
%-----------------

%path(A, B, N), N includes the possible paths that lead from A to B
path(A, B, [A, B]) :- edge(A, B).
path(A, B, [A | XS]) :- edge(A, C), path(C, B, XS).

%Additional Challenge
%Knowledge Base
edge(1,2,1).
edge(1,4,3.5).
edge(1,3,2.5).
edge(2,3,1).
edge(2,5,2.5).
edge(3,4,1).
edge(3,5,2.2).
edge(4,5,1).

%pathLength(A, B, Path, Length) , finds a path between A and B (Path) and calculates the length of the path
pathLength(A, B, [A, B], Length) :- edge(A, B, Length).
pathLength(A, B, [A | XS], Length) :- edge(A, C, N), pathLength(C, B, XS, M), M > 0 , Length is (N + M).

%shortestPath(A, B, N), N is the SHORTEST path between A and B.
shortestPath(A, B, N) :- pathLength(A, B, N, Shortest), \+ (pathLength(A, B, M, Length), Length < Shortest), !.