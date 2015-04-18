CM.make "sources.cm";
Control.Print.printDepth := 10000;
Control.Print.stringDepth := 10000;
Control.Print.printLength := 10000;

fun loadFile x = ((Interpreter.loadProgram (Fol.parse_file x));(print "Successfully loaded.\n"))
fun loadString x = ((Interpreter.loadProgram (Fol.parse_string x));(print "Successfully loaded.\n"))
fun queryString x = Interpreter.answerQuery (Fol.parse_string x);
fun queryFile x = Interpreter.answerQuery (Fol.parse_file x);
fun clear () = Interpreter.clearProgram ();

(*Interpreter.interpret (Fol.parse_string "append([], T, T). append([X|XS], Y, [X|Z]) :- append(XS, Y, Z). ? append([a], L, [a, b, c]). accRev([H|T],M,R):- accRev(T,[H|M],R). accRev([],A,A). naiverev(D, E) :- accRev(D, [], E). ? naiverev([a, b, c, d], L).;");
Interpreter.interpret (Fol.parse_string "edge(a,b). edge(a,g). edge(b,d). edge(c,d). edge(g,c). edge(g,f). edge(c,e). edge(e,d). path(X,X). path(X,Y) :- edge(X,Y).path(X,Y) :- edge(X,Z), path(Z,Y). ?path(a, M).;");*)
(*Interpreter.interpret (Fol.parse_string "factorial(0,1). factorial(A,B) :- greater(A, 0), is(C, A-1), factorial(C, D), is(B, A*D).? factorial(3,X).;");*)