CM.make "sources.cm";
Control.Print.printDepth := 1000;
Fol.parse_string "apple(3.065E-95 + 54) :- apples(Oranges), apple(not Oranges).;";
Fol.parse_string "apples(O) :- lol(Lol) , app(not App, fun(App), s). ;";

Fol.parse_string "apples(A):- oranges(B) , oran(C) .;";

Fol.parse_string "app(True and False).;";

Fol.parse_string "app(A,B).;";
Fol.parse_string "app( ((45/34) + 26) - 23.04 + 23.2E2 + 45 exp 5 ).;";
Fol.parse_string "em(2 exp 3 exp 4).;";
Fol.parse_string "app(Orange).;";
Fol.parse_string "app(Apples and Orange).;";
Fol.parse_string "app( not Applies ).;";