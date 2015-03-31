datatype term = Symbol of string | Func of string * term list | Var of string;
datatype sym = Node of string | Variable of string;
datatype Signature = Sig of (sym * int) list;
datatype subs = Subst of (string * term) list;
exception FailUnify;

Control.Print.printDepth := 100;

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
	fun wff (sign) (Var(x)) = find(Variable(x), sign) = 0
	|   wff (sign) (Symbol(x)) = find(Node(x), sign) = 0
	|   wff (sign) (Func(x, y)) = find(Node(x), sign) = List.length(y) andalso List.all (wff (sign)) (y);	
end

local
	fun mapping (x, Subst([])) = Var(x)
	|   mapping (x, Subst((y,z)::ys)) = if (x = y) then z else mapping(x, Subst(ys));
in
	fun subst (s) (Var(x)) = mapping(x, s)
	|   subst (s) (Symbol(x)) = Symbol(x)
	|   subst (s) (Func(x, y)) = Func(x, List.map (subst (s)) (y));
end

local

	fun compose([], l) = l
	|   compose((x,y)::xs, l) = (x, subst (Subst(l)) (y))::compose(xs, l);

	fun hasVar(x, []) = false
	|   hasVar(x, Var(y)::xs) = (x = y) orelse hasVar(x, xs)
	|   hasVar(x, Symbol(y)::xs) = hasVar(x, xs)
	|   hasVar(x, Func(y, z)::xs) = hasVar(x, z) orelse hasVar(x, xs);

	fun mguListBuilder([],[], l) = l
	|   mguListBuilder([], x::xs, l) = raise FailUnify
	|   mguListBuilder(x::xs, [], l) = raise FailUnify
	|   mguListBuilder(x::xs, y::ys, l) = List.foldl (helper) ([]) (ListPair.zip(x::xs, y::ys))

	and mguBuilder(Symbol(x), Symbol(y)) = if x = y then [] else raise FailUnify
	|   mguBuilder(Var(x), Symbol(y)) = [(x, Symbol(y))]
	|   mguBuilder(Symbol(y), Var(x)) = [(x, Symbol(y))]
	|   mguBuilder(Var(x), Var(y)) = if x = y then [] else [(x, Var(y))]
	|   mguBuilder(Symbol(x), Func(y, z)) = raise FailUnify
	|   mguBuilder(Func(y, z), Symbol(x)) = raise FailUnify
	|   mguBuilder(Var(x), Func(y, z)) = if hasVar(x,z) then raise FailUnify else [(x, Func(y, z))]
	|   mguBuilder(Func(y, z), Var(x)) = if hasVar(x,z) then raise FailUnify else [(x, Func(y, z))]
	|   mguBuilder(Func(y, z), Func(a, b)) = if y = a then mguListBuilder(z, b, []) else raise FailUnify
	
	and helper ((x, y), l) = compose(l, mguBuilder(subst (Subst(l)) (x), subst (Subst(l)) (y)));
in
	fun mgu (x) (y) = Subst(mguBuilder(x, y));
	fun mguList(x) = Subst(mguListBuilder(List.take(x, List.length(x)-1), List.drop(x, 1), []));
end