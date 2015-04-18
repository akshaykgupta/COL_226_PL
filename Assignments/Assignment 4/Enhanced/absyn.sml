signature ABSYN =
  sig
	  datatype expr = VAR of string | SYMBOL of string | NUM of int | BOOL of bool | REAL of real*int | FUNC of string * expr list
	  and head = HEAD of expr
	  and body = BODY of expr list
	  and decl = FACT of expr | RULE of head * body | GOAL of expr list
	  and prog = PROG of decl list
	  
	  
  end

structure Absyn :> ABSYN =
   struct
 	  datatype expr = VAR of string | SYMBOL of string | NUM of int | BOOL of bool | REAL of real*int | FUNC of string * expr list
	  and head = HEAD of expr
 	  and body = BODY of expr list
 	  and decl = FACT of expr | RULE of head * body | GOAL of expr list
 	  and prog = PROG of decl list
	  
   end