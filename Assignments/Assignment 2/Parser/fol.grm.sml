functor FolLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Fol_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\019\000\000\000\000\000\
\\001\000\002\000\028\000\000\000\
\\001\000\004\000\027\000\005\000\026\000\006\000\025\000\025\000\024\000\
\\027\000\023\000\028\000\022\000\029\000\021\000\030\000\020\000\000\000\
\\001\000\004\000\027\000\005\000\026\000\006\000\025\000\025\000\024\000\
\\027\000\048\000\028\000\022\000\029\000\021\000\030\000\020\000\000\000\
\\001\000\007\000\010\000\000\000\
\\001\000\009\000\043\000\010\000\042\000\011\000\041\000\012\000\040\000\
\\013\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\021\000\033\000\022\000\032\000\
\\023\000\031\000\024\000\030\000\026\000\068\000\000\000\
\\001\000\025\000\013\000\000\000\
\\001\000\026\000\045\000\000\000\
\\001\000\026\000\069\000\000\000\
\\001\000\027\000\009\000\000\000\
\\071\000\000\000\
\\072\000\027\000\009\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\\078\000\002\000\011\000\000\000\
\\079\000\000\000\
\\080\000\003\000\029\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\003\000\044\000\000\000\
\\084\000\000\000\
\\085\000\009\000\043\000\010\000\042\000\011\000\041\000\012\000\040\000\
\\013\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\021\000\033\000\022\000\032\000\
\\023\000\031\000\024\000\030\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\088\000\025\000\046\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\009\000\043\000\010\000\042\000\011\000\041\000\012\000\040\000\
\\013\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\021\000\033\000\022\000\032\000\
\\023\000\031\000\024\000\030\000\000\000\
\\093\000\009\000\043\000\010\000\042\000\011\000\041\000\012\000\040\000\
\\013\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\021\000\033\000\022\000\032\000\
\\023\000\031\000\024\000\030\000\000\000\
\\094\000\014\000\038\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\021\000\033\000\022\000\032\000\000\000\
\\095\000\014\000\038\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\021\000\033\000\022\000\032\000\000\000\
\\096\000\014\000\038\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\021\000\033\000\022\000\032\000\000\000\
\\097\000\014\000\038\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\021\000\033\000\022\000\032\000\000\000\
\\098\000\014\000\038\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\021\000\033\000\022\000\032\000\000\000\
\\099\000\009\000\043\000\010\000\042\000\011\000\041\000\012\000\040\000\
\\013\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\021\000\033\000\022\000\032\000\
\\023\000\031\000\024\000\030\000\000\000\
\\100\000\009\000\043\000\010\000\042\000\011\000\041\000\012\000\040\000\
\\013\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\021\000\033\000\022\000\032\000\
\\023\000\031\000\024\000\030\000\000\000\
\\101\000\009\000\043\000\010\000\042\000\011\000\041\000\012\000\040\000\
\\013\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\021\000\033\000\022\000\032\000\
\\023\000\031\000\024\000\030\000\000\000\
\\102\000\009\000\043\000\010\000\042\000\011\000\041\000\012\000\040\000\
\\013\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\021\000\033\000\022\000\032\000\
\\023\000\031\000\024\000\030\000\000\000\
\\103\000\009\000\043\000\010\000\042\000\011\000\041\000\012\000\040\000\
\\013\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\021\000\033\000\022\000\032\000\
\\023\000\031\000\024\000\030\000\000\000\
\\104\000\009\000\043\000\010\000\042\000\011\000\041\000\012\000\040\000\
\\013\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\021\000\033\000\022\000\032\000\
\\023\000\031\000\024\000\030\000\000\000\
\\105\000\009\000\043\000\010\000\042\000\011\000\041\000\012\000\040\000\
\\013\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\021\000\033\000\022\000\032\000\
\\023\000\031\000\024\000\030\000\000\000\
\\106\000\014\000\038\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\021\000\033\000\022\000\032\000\000\000\
\\107\000\014\000\038\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\021\000\033\000\022\000\032\000\000\000\
\\108\000\000\000\
\"
val actionRowNumbers =
"\009\000\004\000\017\000\014\000\
\\013\000\011\000\010\000\006\000\
\\009\000\015\000\012\000\002\000\
\\001\000\018\000\019\000\024\000\
\\022\000\007\000\031\000\029\000\
\\026\000\028\000\003\000\030\000\
\\003\000\003\000\016\000\009\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\002\000\021\000\
\\002\000\005\000\027\000\033\000\
\\032\000\020\000\047\000\046\000\
\\045\000\044\000\043\000\042\000\
\\041\000\039\000\040\000\038\000\
\\037\000\036\000\035\000\034\000\
\\023\000\008\000\048\000\025\000\
\\000\000"
val gotoT =
"\
\\001\000\068\000\002\000\006\000\003\000\005\000\004\000\004\000\
\\005\000\003\000\006\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\010\000\003\000\005\000\004\000\004\000\005\000\003\000\
\\006\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\006\000\014\000\007\000\013\000\009\000\012\000\000\000\
\\000\000\
\\000\000\
\\010\000\017\000\011\000\016\000\012\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\045\000\000\000\
\\000\000\
\\012\000\047\000\000\000\
\\012\000\048\000\000\000\
\\000\000\
\\006\000\014\000\007\000\049\000\000\000\
\\012\000\050\000\000\000\
\\012\000\051\000\000\000\
\\012\000\052\000\000\000\
\\012\000\053\000\000\000\
\\012\000\054\000\000\000\
\\012\000\055\000\000\000\
\\012\000\056\000\000\000\
\\012\000\057\000\000\000\
\\012\000\058\000\000\000\
\\012\000\059\000\000\000\
\\012\000\060\000\000\000\
\\012\000\061\000\000\000\
\\012\000\062\000\000\000\
\\012\000\063\000\000\000\
\\010\000\064\000\011\000\016\000\012\000\015\000\000\000\
\\000\000\
\\010\000\065\000\011\000\016\000\012\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 69
val numrules = 38
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | REAL of  (real*int)
 | NUM of  (int) | UCID of  (string) | LCID of  (string)
 | BOOL of  (bool) | expression of  (Absyn.expr)
 | term of  (Absyn.expr) | termlist of  (Absyn.expr list)
 | body of  (Absyn.body) | head of  (Absyn.head)
 | predicatelist of  (Absyn.pred list) | predicate of  (Absyn.pred)
 | rule of  (Absyn.decl) | fact of  (Absyn.decl)
 | dec of  (Absyn.decl) | declist of  (Absyn.decl list)
 | program of  (Absyn.prog)
end
type svalue = MlyValue.svalue
type result = Absyn.prog
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 14))::
(nil
,nil
 $$ (T 11))::
(nil
,nil
 $$ (T 10))::
(nil
,nil
 $$ (T 8))::
(nil
,nil
 $$ (T 9))::
nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "DOT"
  | (T 2) => "COMMA"
  | (T 3) => "NOT"
  | (T 4) => "ABS"
  | (T 5) => "BOOL"
  | (T 6) => "IF"
  | (T 7) => "QUERY"
  | (T 8) => "PLUS"
  | (T 9) => "SUB"
  | (T 10) => "TIMES"
  | (T 11) => "DIV"
  | (T 12) => "MOD"
  | (T 13) => "GREATER"
  | (T 14) => "EXP"
  | (T 15) => "GREATEROREQUAL"
  | (T 16) => "LESSOREQUAL"
  | (T 17) => "EQUAL"
  | (T 18) => "SEMI"
  | (T 19) => "FUN"
  | (T 20) => "NOTEQUAL"
  | (T 21) => "LESS"
  | (T 22) => "AND"
  | (T 23) => "OR"
  | (T 24) => "LEFTPAREN"
  | (T 25) => "RIGHTPAREN"
  | (T 26) => "LCID"
  | (T 27) => "UCID"
  | (T 28) => "NUM"
  | (T 29) => "REAL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 4) $$ 
(T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.declist declist, declist1left, 
declist1right)) :: rest671)) => let val  result = MlyValue.program (
Absyn.PROG(declist))
 in ( LrTable.NT 0, ( result, declist1left, declist1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.dec dec, dec1left, dec1right)) :: rest671))
 => let val  result = MlyValue.declist ([dec])
 in ( LrTable.NT 1, ( result, dec1left, dec1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.declist declist, _, declist1right)) :: ( _, 
( MlyValue.dec dec, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.declist (dec::declist)
 in ( LrTable.NT 1, ( result, dec1left, declist1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.fact fact, fact1left, fact1right)) :: 
rest671)) => let val  result = MlyValue.dec (fact)
 in ( LrTable.NT 2, ( result, fact1left, fact1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.rule rule, rule1left, rule1right)) :: 
rest671)) => let val  result = MlyValue.dec (rule)
 in ( LrTable.NT 2, ( result, rule1left, rule1right), rest671)
end
|  ( 5, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.predicate 
predicate, predicate1left, _)) :: rest671)) => let val  result = 
MlyValue.fact (Absyn.FACT predicate)
 in ( LrTable.NT 3, ( result, predicate1left, DOT1right), rest671)
end
|  ( 6, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.body body, _, _)
) :: _ :: ( _, ( MlyValue.head head, head1left, _)) :: rest671)) =>
 let val  result = MlyValue.rule (Absyn.RULE(head, body))
 in ( LrTable.NT 4, ( result, head1left, DOT1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.predicate predicate, predicate1left, 
predicate1right)) :: rest671)) => let val  result = MlyValue.head (
Absyn.HEAD(predicate))
 in ( LrTable.NT 7, ( result, predicate1left, predicate1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.predicatelist predicatelist, 
predicatelist1left, predicatelist1right)) :: rest671)) => let val  
result = MlyValue.body (Absyn.BODY(predicatelist))
 in ( LrTable.NT 8, ( result, predicatelist1left, predicatelist1right)
, rest671)
end
|  ( 9, ( ( _, ( MlyValue.predicate predicate, predicate1left, 
predicate1right)) :: rest671)) => let val  result = 
MlyValue.predicatelist ([predicate])
 in ( LrTable.NT 6, ( result, predicate1left, predicate1right), 
rest671)
end
|  ( 10, ( ( _, ( MlyValue.predicatelist predicatelist, _, 
predicatelist1right)) :: _ :: ( _, ( MlyValue.predicate predicate, 
predicate1left, _)) :: rest671)) => let val  result = 
MlyValue.predicatelist (predicate::predicatelist)
 in ( LrTable.NT 6, ( result, predicate1left, predicatelist1right), 
rest671)
end
|  ( 11, ( ( _, ( _, _, RIGHTPAREN1right)) :: ( _, ( MlyValue.termlist
 termlist, _, _)) :: _ :: ( _, ( MlyValue.LCID LCID, LCID1left, _)) ::
 rest671)) => let val  result = MlyValue.predicate (
Absyn.PRED(LCID, termlist))
 in ( LrTable.NT 5, ( result, LCID1left, RIGHTPAREN1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.term term, term1left, term1right)) :: 
rest671)) => let val  result = MlyValue.termlist ([term])
 in ( LrTable.NT 9, ( result, term1left, term1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.termlist termlist, _, termlist1right)) :: _
 :: ( _, ( MlyValue.term term, term1left, _)) :: rest671)) => let val 
 result = MlyValue.termlist (term::termlist)
 in ( LrTable.NT 9, ( result, term1left, termlist1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.expression expression, expression1left, 
expression1right)) :: rest671)) => let val  result = MlyValue.term (
expression)
 in ( LrTable.NT 10, ( result, expression1left, expression1right), 
rest671)
end
|  ( 15, ( ( _, ( _, _, RIGHTPAREN1right)) :: ( _, ( MlyValue.termlist
 termlist, _, _)) :: _ :: ( _, ( MlyValue.LCID LCID, LCID1left, _)) ::
 rest671)) => let val  result = MlyValue.term (
Absyn.FUNC(LCID, termlist))
 in ( LrTable.NT 10, ( result, LCID1left, RIGHTPAREN1right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.UCID UCID, UCID1left, UCID1right)) :: 
rest671)) => let val  result = MlyValue.expression (Absyn.VAR UCID)
 in ( LrTable.NT 11, ( result, UCID1left, UCID1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.LCID LCID, LCID1left, LCID1right)) :: 
rest671)) => let val  result = MlyValue.expression (Absyn.SYMBOL LCID)
 in ( LrTable.NT 11, ( result, LCID1left, LCID1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.NUM NUM, NUM1left, NUM1right)) :: rest671))
 => let val  result = MlyValue.expression (Absyn.NUM NUM)
 in ( LrTable.NT 11, ( result, NUM1left, NUM1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.BOOL BOOL, BOOL1left, BOOL1right)) :: 
rest671)) => let val  result = MlyValue.expression (Absyn.BOOL BOOL)
 in ( LrTable.NT 11, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.REAL REAL, REAL1left, REAL1right)) :: 
rest671)) => let val  result = MlyValue.expression (Absyn.REAL REAL)
 in ( LrTable.NT 11, ( result, REAL1left, REAL1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.expression expression, _, expression1right)
) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (Absyn.UNARY("Not", expression))
 in ( LrTable.NT 11, ( result, NOT1left, expression1right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.expression expression, _, expression1right)
) :: ( _, ( _, ABS1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (Absyn.UNARY("Abs", expression))
 in ( LrTable.NT 11, ( result, ABS1left, expression1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("Plus", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("Sub", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("Times", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("Div", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("Mod", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("Exp", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("Greater", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("GreaterOrEqual", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("LessOrEqual", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("Equal", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 33, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("NotEqual", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("Less", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("And", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (
Absyn.BINARY("Or", expression1, expression2))
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 37, ( ( _, ( _, _, RIGHTPAREN1right)) :: ( _, ( 
MlyValue.expression expression, _, _)) :: ( _, ( _, LEFTPAREN1left, _)
) :: rest671)) => let val  result = MlyValue.expression (expression)
 in ( LrTable.NT 11, ( result, LEFTPAREN1left, RIGHTPAREN1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Fol_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun ABS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.BOOL i,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun QUERY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun EXP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATEROREQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSOREQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NOTEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LESS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun LEFTPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun RIGHTPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun LCID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.LCID i,p1,p2))
fun UCID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.UCID i,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.NUM i,p1,p2))
fun REAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.REAL i,p1,p2))
end
end
