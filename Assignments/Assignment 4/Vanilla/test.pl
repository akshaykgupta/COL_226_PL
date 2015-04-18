edge(a,b).
edge(a,g).
edge(b,d).
edge(c,d).
edge(g,c).
edge(g,f).
edge(c,e).
edge(e,d).
path(X,X). path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z), path(Z,Y). 

append([], T, T).
append([X|XS], Y, [X|Z]) :- append(XS, Y, Z).
accRev([H|T],M,R):- accRev(T,[H|M],R).
accRev([],A,A).
naiverev(D, E) :- accRev(D, [], E).

;