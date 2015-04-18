structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 1
val eof = fn () => Tokens.EOF(!pos,!pos)
fun error (e,l,_) = TextIO.output (TextIO.stdOut, String.concat[
	"pos ", (Int.toString l), ": ", e, "\n"])
	
fun lift (x::y) = if x = #"e" orelse x = #"E" then [] else x::lift(y)
| lift [] = []

fun uptoExp (x::y) = if x = #"e" orelse x = #"E" then y else uptoExp(y)
  | uptoExp [] = []

fun getReal s = (getOpt(Real.fromString(implode(lift(explode(s)))), 0.0), getOpt(Int.fromString(implode(lift(uptoExp(explode(s))))), 0))

%%
%header (functor FolLexFun(structure Tokens: Fol_TOKENS));

digit = [0-9];
validno = 0 | ~?[1-9]{digit}*;
realno = 0\.0 | ~?[1-9]{digit}*\.0 | ~?0\.{digit}*[1-9] | ~?[1-9]{digit}*\.{digit}*[1-9];
realexpno = {realno}[Ee](0 | ~?[1-9]{digit}*);
badno = ~?0+{digit}+ | ~?0+{digit}+\.{digit}* | ~?{digit}*\.{digit}+0+ | -0 | -0.0 | \.{digit}+ | {digit}+\.;
ucid = [A-Z][a-zA-Z0-9_\']*;
lcid = [a-z][a-zA-Z0-9_\']*;
ws=[\t\ ]*;
%%
<INITIAL>{ws}+	     => (lex());
<INITIAL>\n	         => (pos := (!pos) + 1; lex());
<INITIAL>"+"         => (Tokens.PLUS(!pos, !pos));
<INITIAL>"-"         => (Tokens.SUB(!pos, !pos));
<INITIAL>"*"         => (Tokens.TIMES(!pos, !pos));
<INITIAL>"div"       => (Tokens.DIV(!pos, !pos));
<INITIAL>"mod"       => (Tokens.MOD(!pos, !pos));
<INITIAL>"/"         => (Tokens.BY(!pos, !pos));
<INITIAL>"abs"       => (Tokens.ABS(!pos, !pos));
<INITIAL>"exp"       => (Tokens.EXP(!pos, !pos));
<INITIAL>"("		 => (Tokens.LEFTPAREN(!pos, !pos));
<INITIAL>")"    	 => (Tokens.RIGHTPAREN(!pos, !pos));
<INITIAL>","		 => (Tokens.COMMA(!pos, !pos));
<INITIAL>">"		 => (Tokens.GREATER(!pos, !pos));
<INITIAL>">="	     => (Tokens.GREATEROREQUAL(!pos, !pos));
<INITIAL>"<="	     => (Tokens.LESSOREQUAL(!pos, !pos));
<INITIAL>"="		 => (Tokens.EQUAL(!pos, !pos));
<INITIAL>"<>"	     => (Tokens.NOTEQUAL(!pos, !pos));
<INITIAL>"<"		 => (Tokens.LESS(!pos, !pos));
<INITIAL>"and"       => (Tokens.AND(!pos, !pos));
<INITIAL>"or"        => (Tokens.OR(!pos, !pos));
<INITIAL>"not"       => (Tokens.NOT(!pos, !pos));
<INITIAL>"."         => (Tokens.DOT(!pos,!pos));
<INITIAL>"?"         => (Tokens.QUERY(!pos, !pos));
<INITIAL>"_"         => (Tokens.UNDERSCORE(!pos, !pos));
<INITIAL>"|"         => (Tokens.CONS(!pos, !pos));
<INITIAL>"["         => (Tokens.STARTLIST(!pos, !pos));
<INITIAL>"]"         => (Tokens.ENDLIST(!pos, !pos));
<INITIAL>";"         => (Tokens.SEMI(!pos,!pos));
<INITIAL>":-"        => (Tokens.IF(!pos, !pos));
<INITIAL>"true"      => (Tokens.BOOL(valOf(Bool.fromString yytext),!pos,!pos));
<INITIAL>"false"     => (Tokens.BOOL(valOf(Bool.fromString yytext),!pos,!pos));
<INITIAL>{ucid}      => (Tokens.UCID (yytext,!pos,!pos));
<INITIAL>{lcid}      => (Tokens.LCID (yytext,!pos,!pos));
<INITIAL>{validno}   => (Tokens.NUM (valOf(Int.fromString yytext),!pos,!pos));
<INITIAL>{badno}     => (error ("Invalid number/real : " ^ yytext, !pos, !pos); lex());
<INITIAL>({badno}|(0 | -?[1-9]{digit}*))[Ee]({badno}|{realno}|{digit}+) => (error ("Invalid number/real : " ^ yytext, !pos, !pos); lex());
<INITIAL>({realno}[Ee]{realno}) | ({realno}[Ee]{badno}) => (error ("Invalid number/real : " ^ yytext, !pos, !pos); lex());
{realno}             => (Tokens.REAL((valOf(Real.fromString yytext), 0),!pos,!pos));
{realexpno}          => (Tokens.REAL(getReal yytext,!pos,!pos));
<INITIAL>.	         => (error ("Ignoring illegal character : " ^ yytext, !pos, !pos); lex());
