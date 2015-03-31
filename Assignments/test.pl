factorial(0,1).
  
factorial(A,B) :-  
           C is A-1,
           factorial(C,D),
           B is A*D,
           A > 0.
		   
takeout(A,[A|B],B).
takeout(A,[B|C],[B|D]) :-
		   takeout(A,C,D).

edge(a,b).
edge(a,g).
edge(b,d).
edge(c,d).
edge(g,c).
edge(g,f).
edge(c,e).
edge(e,d).

path(X,X).
path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z), path(Z,Y).

append([], X, X).
append([X|XS], Y, [X|Z]) :- append(XS, Y, Z).

naiverev([],[]). 
naiverev([H|T],R):-  naiverev(T,RevT),  append(RevT,[H],R).

prefix(L1, L2) :- append(L1, L3, L2).
suffix(L1, L2) :- append(L3, L1, L2).
sublist(L1, L2) :- append(L1, L3, L4), append(L5, L4, L2).
