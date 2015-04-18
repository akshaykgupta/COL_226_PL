has_type( _ , unit, unit) :- !.
has_type( _ , X, int) :- integer(X), !.
has_type( _ , X, real) :- float(X), !.
has_type( _ , true, boolean) :- !.
has_type( _ , false, boolean) :- !.
has_type( _ , X, string) :- string(X), !.
has_type(Gamma, pair(X, Y), star(T1, T2)) :- has_type(Gamma, X, T1), has_type(Gamma, Y, T2), !.

has_type(_, tuple([]), cross([unit])) :- !.
has_type(Gamma, tuple([X]), cross([T])) :- has_type(Gamma, X, T), !.
has_type(Gamma, tuple([X|XS]), Y) :- has_type(Gamma, X, T), has_type(Gamma, tuple(XS), cross(TS)), Y=cross([T|TS]), !.

has_type(Gamma, lambda((X), E), arrow(T1, T2)) :- has_type([((X), T1)|Gamma], E, T2), !.

has_type(Gamma, add(X1, X2), int) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int), !.
has_type(Gamma, sub(X1, X2), int) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int), !.
has_type(Gamma, mul(X1, X2), int) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int), !.
has_type(Gamma, divide(X1, X2), int) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int).
has_type(Gamma, modulo(X1, X2), int) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int), !.
has_type(Gamma, exp(X1, X2), int) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int), !.
has_type(Gamma, absol(X), int) :- has_type(Gamma, X, int), !.

has_type(Gamma, greater(X1, X2), boolean) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int), !.
has_type(Gamma, greaterorequal(X1, X2), boolean) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int), !.
has_type(Gamma, less(X1, X2), boolean) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int), !.
has_type(Gamma, lessorequal(X1, X2), boolean) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int), !.
has_type(Gamma, equals(X1, X2), boolean) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int), !.
has_type(Gamma, notequals(X1, X2), boolean) :- has_type(Gamma, X1, int), has_type(Gamma, X2, int), !.

has_type(Gamma, add(X1, X2), real) :- has_type(Gamma, X1, real), has_type(Gamma, X2, real), !.
has_type(Gamma, sub(X1, X2), real) :- has_type(Gamma, X1, real), has_type(Gamma, X2, real), !.
has_type(Gamma, mul(X1, X2), real) :- has_type(Gamma, X1, real), has_type(Gamma, X2, real), !.
has_type(Gamma, divide(X1, X2), real) :- has_type(Gamma, X1, real), has_type(Gamma, X2, real), !.
has_type(Gamma, exp(X1, X2), real) :- has_type(Gamma, X1, real), has_type(Gamma, X2, real), !.
has_type(Gamma, absol(X), real) :- has_type(Gamma, X, real).

has_type(Gamma, greater(X1, X2), boolean) :- has_type(Gamma, X1, real), has_type(Gamma, X2, real), !.
has_type(Gamma, greaterorequal(X1, X2), boolean) :- has_type(Gamma, X1, real), has_type(Gamma, X2, real), !.
has_type(Gamma, less(X1, X2), boolean) :- has_type(Gamma, X1, real), has_type(Gamma, X2, real), !.
has_type(Gamma, lessorequal(X1, X2), boolean) :- has_type(Gamma, X1, real), has_type(Gamma, X2, real), !.
has_type(Gamma, equals(X1, X2), boolean) :- has_type(Gamma, X1, real), has_type(Gamma, X2, real), !.
has_type(Gamma, notequals(X1, X2), boolean) :- has_type(Gamma, X1, real), has_type(Gamma, X2, real), !.

has_type(Gamma, concat(X1, X2), string) :- has_type(Gamma, X1, string), has_type(Gamma, X2, string), !.

has_type(Gamma, and(X1, X2), boolean) :- has_type(Gamma, X1, boolean), has_type(Gamma, X2, boolean), !.
has_type(Gamma, or(X1, X2), boolean) :- has_type(Gamma, X1, boolean), has_type(Gamma, X2, boolean), !.
has_type(Gamma, lognot(X), boolean) :- has_type(Gamma, X, boolean), !.

has_type(Gamma, if(E, E1, E2), T) :- has_type(Gamme, E, boolean), has_type(Gamme, E1, T), has_type(Gamma, E2, T), !.
has_type(Gamma, let(X, E), T) :- elaborates(Gamma, X, GammaPrime), append(Gamma, GammaPrime, NewGamma), has_type(NewGamma, E, T), !.
has_type(Gamma, apply(E1, E2), T) :- has_type(Gamma, E1, arrow(T1, T)), has_type(Gamma, E2, T1), !.  

has_type(Gamma, (X), T1) :- blob(X, text), lookup(Gamma, (X), T1), !.

elaborates(Gamma, define(X, E), [(X, T)]) :- has_type(Gamma, E, T), !.
elaborates(Gamma, define(tuple(X), tuple(E)), [(tuple(X), T)]) :- has_type(Gamma, tuple(E), T), !.
elaborates(Gamma, parallel([X|XS]), Y) :- elaborates(Gamma, X, Z), elaborates(Gamma, parallel(XS), ZS), append(Z, ZS, Y), !. 
elaborates(Gamma, series([]), []) :- !.
elaborates(Gamma, series([X|XS]), M) :- elaborates(Gamma, X, Y), append(Gamma, Y, NewGamma), elaborates(NewGamma, series(XS), YS), append(Y, YS, M), !. 
elaborates(Gamma, local(X, Y), M) :- elaborates(Gamma, X, GammaPrime), append(Gamma, GammaPrime, NewGamma), elaborates(NewGamma, Y, M), !.

augment(M, [], M) :- !.
augment([], M, M) :- !.
augment([((X), T)|XS], [((X), T1)|YS], M) :- augment(XS, YS, M), !.
augment([((X), T)|XS], [((Y), T1)|YS], M) :- augment(XS, [((Y), T1)|YS], N), augment([((X), T)], YS, O), append(N, O, M), !.

append(M, [], M) :- !.
append(M, [X|XS], [X|YS]) :- append(M, XS, YS), !.

lookup([(X,T)|Y], X, T):- !.
lookup([(tuple([X|ZS]), cross([T|MS]))|Y], X, T) :- !.
lookup([(tuple([Z|ZS]), cross([M|MS]))|Y], X, T) :- lookup([(tuple(ZS), cross(MS))|Y], X, T), !.
lookup([(Z,T1)|Y], X, T) :- lookup(Y, X, T), !.

smalllookup([(X,T)|Y], X, T):- !.
smalllookup([(tuple([X|ZS]), tuple([T|MS]))|Y], X, T) :- !.
smalllookup([(tuple([Z|ZS]), tuple([M|MS]))|Y], X, T) :- smalllookup([(tuple(ZS), tuple(MS))|Y], X, T), !.
smalllookup([(Z,T1)|Y], X, T) :- smalllookup(Y, X, T), !.

%calculates starts here
calculates(SmallGamma, X, X) :- integer(X), !.
calculates(SmallGamma, X, X) :- float(X), !.
calculates(SmallGamma, X, X) :- string(X), !.
calculates(SmallGamma, true, true) :- !.
calculates(SmallGamma, false, false) :- !.
calculates(SmallGamma, unit, unit) :- !.

calculates(SmallGamma, tuple([]), unit) :- !.
calculates(SmallGamma, tuple([X]), tuple([V])) :- calculates(SmallGamma, X, V), !.
calculates(SmallGamma, tuple([X|XS]), Y) :- calculates(SmallGamma, X, V), calculates(SmallGamma, tuple(XS), tuple(VS)), Y=tuple([V|VS]), !.

calculates(SmallGamma, add(X1, X2), Ans) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), Ans is V1+V2, !.
calculates(SmallGamma, sub(X1, X2), Ans) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), Ans is V1-V2, !.
calculates(SmallGamma, mul(X1, X2), Ans) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), Ans is V1*V2, !.
calculates(SmallGamma, divide(X1, X2), Ans) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), integer(V1), integer(V2), Ans is div(V1, V2), !.
calculates(SmallGamma, divide(X1, X2), Ans) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), float(V1), float(V2), Ans is V1/V2, !.
calculates(SmallGamma, modulo(X1, X2), Ans) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), Ans is mod(V1, V2), !.
calculates(SmallGamma, exp(X1, X2), Ans) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), Ans is V1^V2, !.
calculates(SmallGamma, absol(X), Ans) :- calculates(SmallGamma, X, V), Ans is abs(V), !.
calculates(SmallGamma, concat(X1, X2), Ans) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), string_concat(V1, V2, Ans), !.

calculates(SmallGamma, greater(X1, X2), true) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), V1>V2, !.
calculates(SmallGamma, greaterorequal(X1, X2), true) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), V1>=V2, !.
calculates(SmallGamma, less(X1, X2), true) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), V1<V2, !.
calculates(SmallGamma, lessorequal(X1, X2), true) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), V1=<V2, !.
calculates(SmallGamma, equals(X1, X2), true) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), V1=V2, !.
calculates(SmallGamma, notequals(X1, X2), true) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), not(V1=V2), !.

calculates(SmallGamma, greater(X1, X2), false) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), not(V1>V2), !.
calculates(SmallGamma, greaterorequal(X1, X2), false) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), not(V1>=V2), !.
calculates(SmallGamma, less(X1, X2), false) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), not(V1<V2), !.
calculates(SmallGamma, lessorequal(X1, X2), false) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), not(V1=<V2), !.
calculates(SmallGamma, equals(X1, X2), false) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), not(V1=V2), !.
calculates(SmallGamma, notequals(X1, X2), false) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), V1=V2, !.

calculates(SmallGamma, and(X1, X2), true) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), V1=true, V2=true, !.
calculates(SmallGamma, or(X1, X2), true) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), (V1=true; V2=true), !.
calculates(SmallGamma, lognot(X), true) :- calculates(SmallGamma, X, V), V=false, !.

calculates(SmallGamma, and(X1, X2), false) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), (V1=true; V2=true), !.
calculates(SmallGamma, or(X1, X2), false) :- calculates(SmallGamma, X1, V1), calculates(SmallGamma, X2, V2), V1=false, V2=false, !.
calculates(SmallGamma, lognot(X), false) :- calculates(SmallGamma, X, V), V=false, !.

calculates(SmallGamma, if(X, E1, E2), Ans) :- calculates(SmallGamma, X, true), calculates(SmallGamma, E1, Ans), !.
calculates(SmallGamma, if(X, E1, E2), Ans) :- calculates(SmallGamma, X, false), calculates(SmallGamma, E2, Ans), !.
calculates(SmallGamma, let(X, E), Ans) :- defines(SmallGamma, X, SmallGammaPrime), append(SmallGamma, SmallGammaPrime, NewSmallGamma), calculates(NewSmallGamma, E, Ans), !.

calculates(SmallGamma, closure(SmallGamma, lambda(X, E)), closure(SmallGamma, lambda(X, E))) :- !.
calculates(SmallGamma, lambda(X, E), closure(SmallGamma, lambda(X, E))) :- !.
calculates(SmallGamma, apply(E1, E2), Ans) :- calculates(SmallGamma, E1, closure(OldGamma, lambda(X, E))), calculates(OldGamma, E2, V), defines(SmallGamma, define(X, V), SmallGammaPrime), append(SmallGamma, SmallGammaPrime, NewSmallGamma), calculates(NewSmallGamma, E, Ans), !.

calculates(SmallGamma, (X), V) :- blob(X, text), smalllookup(SmallGamma, (X), V), !.

defines(SmallGamma, define((X), E), [((X), V)]) :- calculates(SmallGamma, E, V), !.
defines(SmallGamma, define(tuple(X), tuple(E)), [(tuple(X), V)]) :- calculates(SmallGamma, tuple(E), V), !.
defines(SmallGamma, parallel([X|XS]), SmallGammaPrime) :- defines(SmallGamma, X, Z), defines(SmallGamma, XS, ZS), append(Z, ZS, SmallGammaPrime), !.
defines(SmallGamma, series([]), []) :- !.
defines(SmallGamma, series([X|XS]), M) :- defines(SmallGamma, X, Y), append(SmallGamma, Y, NewSmallGamma), defines(NewSmallGamma, series(XS), YS), append(Y, YS, M), !.
defines(SmallGamma, local(X, Y), M) :- defines(SmallGamma, X, SmallGammaPrime), append(SmallGamma, SmallGammaPrime, NewSmallGamma), defines(NewSmallGamma, Y, M), !.

subjectReduction(Gamma, SmallGamma, E) :- has_type(Gamma, E, T), calculates(SmallGamma, E, V), has_type(Gamma, V, T).
