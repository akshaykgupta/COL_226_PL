structure Prolog = struct
	
	(*datatype term = Symbol of string | Func of string * term list | Var of string;
	datatype sym = Node of string | Variable of string;
	datatype Signature = Sig of (sym * int) list;
	datatype subs = Subst of (string * term) list;*)
	
	datatype sym = Node of string | Variable of string;
	datatype Signature = Sig of (sym * int) list;
	datatype subs = Subst of (string * Absyn.expr) list;
	exception FailUnify;
	exception FailEval;
	local
		fun isRepeated ((Node(x),y),[]) = true
		|   isRepeated ((Variable(x), y), []) = if y <> 0 then false else true
		|   isRepeated ((Variable(x),y), (Node(a),b)::xs) = if (x = a) orelse y <> 0 then false else isRepeated((Variable(x),y), xs)
		|   isRepeated ((Variable(x), y), (Variable(a),b)::xs) = if y <> 0 then false else isRepeated((Variable(x),y), xs)
		|   isRepeated ((Node(x),y), (Variable(a),b)::xs) = if (x = a) then false else isRepeated((Node(x),y), xs)
		|   isRepeated ((Node(x),y), (Node(a),b)::xs) = if (x = a) andalso y <> b then false else isRepeated((Node(a),b), xs);
	in
		fun check_sig (Sig([])) = true
		|   check_sig (Sig((x, y)::xs)) = if (y < 0) then false else isRepeated((x, y), xs) andalso check_sig(Sig(xs));
	end

	local
		fun find(x, Sig([])) = ~1
		|   find(x, Sig((a,b)::xs)) = if (x = a) then b else find(x, Sig(xs));
	in
		fun wff (sign) (Absyn.VAR(x)) = find(Variable(x), sign) = 0
		|   wff (sign) (Absyn.SYMBOL(x)) = find(Node(x), sign) = 0
		|   wff (sign) (Absyn.FUNC(x, y)) = find(Node(x), sign) = List.length(y) andalso List.all (wff (sign)) (y);	
	end

	local
		fun mapping (x, Subst([])) = Absyn.VAR(x)
		|   mapping (x, Subst((y,z)::ys)) = if (x = y) then z else mapping(x, Subst(ys));
		
		fun toStringList ([A, Absyn.SYMBOL "[]"]) = toStringExpr A
		|	toStringList ([A, (Absyn.FUNC("cons", x))]) = (toStringExpr A) ^ ", " ^ (toStringList x)
		|   toStringList ([A, Absyn.VAR B]) = (toStringExpr A) ^ "|" ^ B
		
  	  	and toStringExpr (Absyn.VAR x) = x
  	  	|	toStringExpr (Absyn.SYMBOL x) = x
  	  	|   toStringExpr (Absyn.NUM x) = Int.toString x
  	  	|   toStringExpr (Absyn.BOOL x) = Bool.toString x
		|   toStringExpr (Absyn.FUNC("cons", [A, Absyn.SYMBOL "[]"])) = "[" ^ (toStringExpr A) ^ "]"
		|   toStringExpr (Absyn.FUNC("cons", x)) = "[" ^ (toStringList x) ^ "]"
  	  	|   toStringExpr (Absyn.FUNC(x, y)) = x ^ "(" ^ (toStringExprList y) ^ ")"
		
		and toStringExprList ([]) = ""
		|	toStringExprList ([x]) = toStringExpr x
		|   toStringExprList (x::xs) = (toStringExpr x) ^ ", " ^ (toStringExprList xs);
		
		fun toStringSub [] = ""
		|	toStringSub [(x, y)] = x ^ " = " ^ (toStringExpr y)
		|	toStringSub ((x, y)::xs) = x ^ " = " ^ (toStringExpr y) ^ ", " ^ (toStringSub xs);
	in
		fun subst (s) (Absyn.VAR(x)) = mapping(x, s)
		|   subst (s) (Absyn.SYMBOL(x)) = Absyn.SYMBOL(x)
		|   subst (s) (Absyn.NUM(x)) = Absyn.NUM(x)
		|   subst (s) (Absyn.REAL(x)) = Absyn.REAL(x)
		|   subst (s) (Absyn.FUNC(x, y)) = Absyn.FUNC(x, List.map (subst (s)) (y));
		
  	  	fun subToString (Subst []) = "true"
		|	subToString (Subst x) = toStringSub x;
		
  	  	fun subListToString [] = ""
		|	subListToString (x::xs) = (subToString x) ^ " ;\n" ^ (subListToString xs);
		
	end
	
	local
		fun implode [] = []
		|	implode (x::xs) = x@(implode xs)
	in
		fun vars (Absyn.VAR x) = [x]
		|	vars (Absyn.SYMBOL x) = []
		|	vars (Absyn.NUM x) = []
		|	vars (Absyn.REAL x) = []
		|   vars (Absyn.FUNC(x, y)) = implode (map vars y);
		
		fun varslist x = implode (map vars x);
		
		fun append a (Absyn.VAR x) = (Absyn.VAR (x^a))
		|	append a (Absyn.SYMBOL x) = (Absyn.SYMBOL x)
		|	append a (Absyn.NUM x) = (Absyn.NUM x)
		|	append a (Absyn.REAL x) = (Absyn.REAL x)
		|   append a (Absyn.FUNC(x, y)) = (Absyn.FUNC(x, (map (append a) y)));
		 
	end
	
	local
		fun contains ([]) y = false
		|	contains (x::xs) (y) = if (x = y) then true else (contains xs y)
		
		fun filterSub x [] = []
		|	filterSub x ((y, z)::ys) = if contains x y then (y, z)::(filterSub x ys) else filterSub x ys
	in
		fun subFilter x (Subst y) = Subst (filterSub x y);
	end

	local
		fun lcompose [] l = l
		|   lcompose ((x,y)::xs) l = (x, subst (Subst(l)) (y))::(lcompose xs l);
		
		fun hasVar(x, []) = false
		|   hasVar(x, Absyn.VAR(y)::xs) = (x = y) orelse hasVar(x, xs)
		|   hasVar(x, Absyn.SYMBOL(y)::xs) = hasVar(x, xs)
		|   hasVar(x, Absyn.NUM(y)::xs) = hasVar(x, xs)
		|   hasVar(x, Absyn.REAL(y)::xs) = hasVar(x, xs)
		|   hasVar(x, Absyn.FUNC(y, z)::xs) = hasVar(x, z) orelse hasVar(x, xs);

		fun mguListBuilder([],[], l) = l
		|   mguListBuilder([], x::xs, l) = raise FailUnify
		|   mguListBuilder(x::xs, [], l) = raise FailUnify
		|   mguListBuilder(x::xs, y::ys, l) = List.foldl (helper) ([]) (ListPair.zip(x::xs, y::ys))

		and mguBuilder(Absyn.SYMBOL(x), Absyn.SYMBOL(y)) = if x = y then [] else raise FailUnify
		|   mguBuilder(Absyn.NUM(x), Absyn.NUM(y)) = if x = y then [] else raise FailUnify
		|   mguBuilder(Absyn.SYMBOL(x), Absyn.NUM(y)) = raise FailUnify
		|   mguBuilder(Absyn.NUM(x), Absyn.SYMBOL(y)) = raise FailUnify
		|   mguBuilder(Absyn.VAR(x), Absyn.SYMBOL(y)) = [(x, Absyn.SYMBOL(y))]
		|   mguBuilder(Absyn.SYMBOL(y), Absyn.VAR(x)) = [(x, Absyn.SYMBOL(y))]
		|   mguBuilder(Absyn.VAR(x), Absyn.NUM(y)) = [(x, Absyn.NUM(y))]
		|   mguBuilder(Absyn.NUM(y), Absyn.VAR(x)) = [(x, Absyn.NUM(y))]
		|   mguBuilder(Absyn.VAR(x), Absyn.VAR(y)) = if x = y then [] else [(x, Absyn.VAR(y))]
		|   mguBuilder(Absyn.SYMBOL(x), Absyn.FUNC(y, z)) = raise FailUnify
		|   mguBuilder(Absyn.FUNC(y, z), Absyn.SYMBOL(x)) = raise FailUnify
		|   mguBuilder(Absyn.NUM(x), Absyn.FUNC(y, z)) = raise FailUnify
		|   mguBuilder(Absyn.FUNC(y, z), Absyn.NUM(x)) = raise FailUnify
		|   mguBuilder(Absyn.VAR(x), Absyn.FUNC(y, z)) = if hasVar(x,z) then raise FailUnify else [(x, Absyn.FUNC(y, z))]
		|   mguBuilder(Absyn.FUNC(y, z), Absyn.VAR(x)) = if hasVar(x,z) then raise FailUnify else [(x, Absyn.FUNC(y, z))]
		|   mguBuilder(Absyn.FUNC(y, z), Absyn.FUNC(a, b)) = if y = a andalso (List.length z = List.length b) then mguListBuilder(z, b, []) else raise FailUnify
	
		and helper ((x, y), l) = lcompose l (mguBuilder(subst (Subst(l)) (x), subst (Subst(l)) (y)));
	in
		fun mgu (x) (y) = SOME (Subst(mguBuilder(x, y))) handle FailUnify => NONE;
		fun mguList(x) = SOME (Subst(mguListBuilder(List.take(x, List.length(x)-1), List.drop(x, 1), []))) handle FailUnify => NONE;
		fun compose (Subst x) (Subst y) = (Subst (lcompose x y));
	end
	
	local
		fun eval (Absyn.NUM x) = Absyn.NUM x
		|   eval (Absyn.FUNC("plus", [Absyn.NUM x, Absyn.NUM y])) = Absyn.NUM (x + y)
		|	eval (Absyn.FUNC("sub", [Absyn.NUM x, Absyn.NUM y])) = Absyn.NUM (x - y)
		|   eval (Absyn.FUNC("times", [Absyn.NUM x, Absyn.NUM y])) = Absyn.NUM (x * y)
		|   eval (Absyn.FUNC("div", [Absyn.NUM x, Absyn.NUM y])) = Absyn.NUM (x div y)
		|   eval (Absyn.FUNC("mod", [Absyn.NUM x, Absyn.NUM y])) = Absyn.NUM (x mod y)
		|   eval (Absyn.FUNC("plus", [Absyn.NUM x, Absyn.FUNC y])) = eval (Absyn.FUNC("plus", [Absyn.NUM x, (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("sub", [Absyn.NUM x, Absyn.FUNC y])) = eval (Absyn.FUNC("sub", [Absyn.NUM x, (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("times", [Absyn.NUM x, Absyn.FUNC y])) = eval (Absyn.FUNC("times", [Absyn.NUM x, (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("div", [Absyn.NUM x, Absyn.FUNC y])) = eval (Absyn.FUNC("div", [Absyn.NUM x, (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("mod", [Absyn.NUM x, Absyn.FUNC y])) = eval (Absyn.FUNC("mod", [Absyn.NUM x, (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("plus", [Absyn.FUNC y, Absyn.NUM x])) = eval (Absyn.FUNC("plus", [Absyn.NUM x, (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("sub",  [Absyn.FUNC y, Absyn.NUM x])) = eval (Absyn.FUNC("sub", [Absyn.NUM x, (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("times", [Absyn.FUNC y, Absyn.NUM x])) = eval (Absyn.FUNC("times", [Absyn.NUM x, (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("div", [Absyn.FUNC y, Absyn.NUM x])) = eval (Absyn.FUNC("div", [Absyn.NUM x, (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("mod", [Absyn.FUNC y, Absyn.NUM x])) = eval (Absyn.FUNC("mod", [Absyn.NUM x, (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("plus", [Absyn.FUNC x, Absyn.FUNC y])) = eval (Absyn.FUNC("plus", [(eval (Absyn.FUNC x)), (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("sub", [Absyn.FUNC x, Absyn.FUNC y])) = eval (Absyn.FUNC("sub", [(eval (Absyn.FUNC x)), (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("times", [Absyn.FUNC x, Absyn.FUNC y])) = eval (Absyn.FUNC("times", [(eval (Absyn.FUNC x)), (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("div", [Absyn.FUNC x, Absyn.FUNC y])) = eval (Absyn.FUNC("div", [(eval (Absyn.FUNC x)), (eval (Absyn.FUNC y))]))
		|   eval (Absyn.FUNC("mod", [Absyn.FUNC x, Absyn.FUNC y])) = eval (Absyn.FUNC("mod", [(eval (Absyn.FUNC x)), (eval (Absyn.FUNC y))]))	
		|   eval _ = raise FailEval;
		
		fun greater (Absyn.NUM x) (Absyn.NUM y) = if x > y then true else false
		
		fun equals (Absyn.NUM x) (Absyn.NUM y) = x = y
		|   equals _ _ = false
	in
		fun mguEval (Absyn.FUNC("is", [Absyn.VAR x, y])) z = SOME (Subst ([(x, (eval y))]))
		|   mguEval z (Absyn.FUNC("is", [Absyn.VAR x, y])) = SOME (Subst ([(x, (eval y))]))
		|   mguEval (Absyn.FUNC("is", [x, y])) z = if equals x (eval y) then SOME (Subst []) else NONE
		|   mguEval z (Absyn.FUNC("is", [x, y])) = if equals x (eval y) then SOME (Subst []) else NONE
		|   mguEval (Absyn.FUNC("greater", [x, y])) z = if greater (eval x) (eval y) then SOME (Subst []) else NONE
		|   mguEval z (Absyn.FUNC("greater", [x, y])) = if greater (eval x) (eval y) then SOME (Subst []) else NONE
		|   mguEval y z = mgu y z;
	end
	
end