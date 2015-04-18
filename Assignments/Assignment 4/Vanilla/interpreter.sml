structure Interpreter = struct
	open Absyn;
	open Prolog;
	
	val cnt = ref(0)
	
	fun changeScope [] = []
	|   changeScope (FACT(y)::xs) = (FACT(append (Int.toString (!cnt)) y)::xs)
	|   changeScope (GOAL(y)::xs) = (GOAL(map (append (Int.toString (!cnt))) y)::xs)
	|   changeScope (RULE(HEAD(y), BODY(z))::xs) = (RULE(HEAD(append (Int.toString (!cnt)) y), BODY(map (append (Int.toString (!cnt))) z))::xs);
	
	fun unify x [] l = []
	|   unify x (FACT(y)::xs) l = let
									 val r = (mguEval y x)
								  in
									 if isSome r then (valOf r)::(unify x xs l) else (unify x xs l)
								  end
	|   unify x (GOAL(y)::xs) l = (unify x xs l)
	|   unify x (RULE(HEAD(y), BODY(z))::xs) l = let
													val r = mguEval y x
													val s = if isSome r then (map (compose (valOf r))(ruleUnify (List.map (subst (valOf r)) z) l)) else []
												 in
													s@(unify x xs l)
												 end
	
	and ruleUnify [] l = []
	|   ruleUnify [y] l = let
						  	  val m = ((cnt := !cnt + 1); (unify y (changeScope l) l))
						  in
							  m
						  end
	|   ruleUnify (y::ys) l = let
							  	  val m = ((cnt := !cnt + 1); (unify y (changeScope l) l));
								  fun unifyEach [] = []
								  |   unifyEach (x::xs) = let
									  						 val f = subFilter (vars y) x
									  						 val r = ruleUnify (List.map (subst f) ys) l
														  in
															 (map (compose f) r)@(unifyEach xs)
														  end
							  in
								  unifyEach m
							  end
														  
	fun interpret p = let
						fun getDecList (PROG x) = x
						
						fun query [] l = []
						|   query (GOAL(x)::xs) l = (map (subFilter (varslist x)) (ruleUnify x l))::(query xs l)
						|   query (FACT(x)::xs) l = query xs l
						|   query (RULE(x)::xs) l = query xs l
						
						fun printI [] = ()
						|	printI (([])::xs) = ((print ("false\n"));(printI xs);(print))
						|   printI (x::xs) = ((print ((subListToString x)^"\n"));(printI xs));
					  in
						printI (query (getDecList p) (getDecList p))
					  end
	
	val program = ref ([] : decl list)
	fun loadProgram (PROG l) = program := (!program)@(l)
	fun answerQuery (PROG l) = interpret (PROG ((!program)@l))
	fun clearProgram () = program := []		  
end