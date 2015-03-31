structure Interpreter = struct
	open Absyn;
	open Prolog;
	fun unify x [] l = []
	|   unify x (FACT(y)::xs) l = if isSome (mguEval y x) then (valOf (mguEval y x))::(unify x xs l) else (unify x xs l)
	|   unify x (GOAL(y)::xs) l = (unify x xs l)
	|   unify x (RULE(HEAD(y), BODY(z))::xs) l = let
													val r = mguEval y x
													val s = if isSome r then (ruleUnify (List.map (subst (valOf r)) z) l) else NONE
												 in
													if isSome s then (valOf s)@(unify x xs l) else (unify x xs l)
												 end
	
	and ruleUnify [] l = NONE
	|   ruleUnify [(y as FUNC("is", [A, B]))] l = if isSome ( mguEval (y) (SYMBOL("[]")) ) then SOME [(valOf(mguEval (y) (SYMBOL("[]"))))] else NONE
	|   ruleUnify (( y as FUNC("is", [A, B]))::ys) l = let
							  	  val m = if isSome ( mguEval (y) (SYMBOL("[]")) ) then SOME [(valOf(mguEval (y) (SYMBOL("[]"))))] else NONE
								  fun unifyEach [] = []
								  |   unifyEach (x::xs) = let
			 						 						 val f = subFilter (vars y) x
									  						 val r = ruleUnify (List.map (subst f) ys) l
														  in
															 if isSome r then (map (compose f) (valOf r))@(unifyEach xs) else (unifyEach xs)
														  end
							  in
								  if (not(isSome m)) then NONE else if (List.null (unifyEach (valOf m))) then NONE else SOME (unifyEach (valOf m))
							  end
  	|   ruleUnify [(y as FUNC("greater", [A, B]))] l = if isSome ( mguEval (y) (SYMBOL("[]")) ) then SOME [(valOf(mguEval (y) (SYMBOL("[]"))))] else NONE
  	|   ruleUnify (( y as FUNC("greater", [A, B]))::ys) l = let
  							  	  val m = if isSome ( mguEval (y) (SYMBOL("[]")) ) then SOME [(valOf(mguEval (y) (SYMBOL("[]"))))] else NONE
  								  fun unifyEach [] = []
  								  |   unifyEach (x::xs) = let
			  						 						 val f = subFilter (vars y) x
  									  						 val r = ruleUnify (List.map (subst f) ys) l
  														  in
  															 if isSome r then (map (compose f) (valOf r))@(unifyEach xs) else (unifyEach xs)
  														  end
  							  in
  								  if (not(isSome m)) then NONE else if (List.null (unifyEach (valOf m))) then NONE else SOME (unifyEach (valOf m))
  							  end

	|   ruleUnify [y] l = let
						  	  val m = unify y l l
						  in
							  if (List.null m) then NONE else SOME m
						  end
	|   ruleUnify (y::ys) l = let
							  	  val m = unify y l l;
								  fun unifyEach [] = []
								  |   unifyEach (x::xs) = let
									  						 val f = subFilter (vars y) x
									  						 val r = ruleUnify (List.map (subst f) ys) l
														  in
															 if isSome r then (map (compose f) (valOf r))@(unifyEach xs) else (unifyEach xs)
														  end
							  in
								  if (List.null m) then NONE else if (List.null (unifyEach m)) then NONE else SOME (unifyEach m)
							  end
														  
	fun interpret p = let
						fun getDecList (PROG x) = x
						
						fun query [] l = []
						|   query (GOAL(x)::xs) l = if isSome (ruleUnify x l) then SOME (map (subFilter (varslist x)) (valOf (ruleUnify x l)))::(query xs l) else NONE::(query xs l)
						|   query (FACT(x)::xs) l = query xs l
						|   query (RULE(x)::xs) l = query xs l
						
						fun printI [] = ()
						|	printI ((NONE)::xs) = ((print ("false\n"));(printI xs))
						|   printI ((SOME x)::xs) = ((print ((subListToString x)^"\n"));(printI xs));
					  in
						printI (query (getDecList p) (getDecList p))
					  end
					  
end