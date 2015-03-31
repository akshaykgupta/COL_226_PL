signature ABSYN =
  sig
	  datatype expr = VAR of string | SYMBOL of string | NUM of int | BOOL of bool | REAL of real*int | UNARY of string * expr | BINARY of string*expr*expr | FUNC of string * expr list
	  and pred = PRED of string * expr list
	  and head = HEAD of pred
	  and body = BODY of pred list
	  and decl = FACT of pred | RULE of head * body
	  and prog = PROG of decl list
  end

structure Absyn :> ABSYN =
   struct
 	  datatype expr = VAR of string | SYMBOL of string | NUM of int | BOOL of bool | REAL of real*int | UNARY of string * expr | BINARY of string*expr*expr | FUNC of string * expr list
 	  and pred = PRED of string * expr list
	  and head = HEAD of pred
 	  and body = BODY of pred list
 	  and decl = FACT of pred | RULE of head * body
 	  and prog = PROG of decl list
   end