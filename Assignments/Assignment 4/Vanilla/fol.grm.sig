signature Fol_TOKENS =
sig
type ('a,'b) token
type svalue
val REAL: (real*int) *  'a * 'a -> (svalue,'a) token
val NUM: (int) *  'a * 'a -> (svalue,'a) token
val UCID: (string) *  'a * 'a -> (svalue,'a) token
val LCID: (string) *  'a * 'a -> (svalue,'a) token
val QUERY:  'a * 'a -> (svalue,'a) token
val ENDLIST:  'a * 'a -> (svalue,'a) token
val STARTLIST:  'a * 'a -> (svalue,'a) token
val CONS:  'a * 'a -> (svalue,'a) token
val UNDERSCORE:  'a * 'a -> (svalue,'a) token
val RIGHTPAREN:  'a * 'a -> (svalue,'a) token
val LEFTPAREN:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val LESS:  'a * 'a -> (svalue,'a) token
val NOTEQUAL:  'a * 'a -> (svalue,'a) token
val FUN:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val EQUAL:  'a * 'a -> (svalue,'a) token
val LESSOREQUAL:  'a * 'a -> (svalue,'a) token
val GREATEROREQUAL:  'a * 'a -> (svalue,'a) token
val BY:  'a * 'a -> (svalue,'a) token
val EXP:  'a * 'a -> (svalue,'a) token
val GREATER:  'a * 'a -> (svalue,'a) token
val MOD:  'a * 'a -> (svalue,'a) token
val DIV:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val SUB:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val BOOL: (bool) *  'a * 'a -> (svalue,'a) token
val ABS:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Fol_LRVALS=
sig
structure Tokens : Fol_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
