datatype lexresult = STRING of string | ID of string | NUM of int | REAL of real*int | EOF | PLUS | SUB | TIMES | DIV | MOD | ABS | EXP | LEFTPAREN | RIGHTPAREN | BOOLEAN of bool | GREATER | LESS | GREATEROREQUAL | LESSOREQUAL | EQUAL | NOTEQUAL | AND | OR | NOT | CONCAT | LET | IF | THEN | ELSE | END | COMMA | COLON | SUBSTRING of lexresult*lexresult

val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = fn () => EOF

fun extract (x::y) = if x <> #"." andalso x<> #"]" then x::extract(y) else []
| extract [] = []

fun upto (x::y) = if x = #"." then y else upto(y)
| upto [] = []

fun until (x::y) = if x = #"." orelse x = #"[" then until(y) else x::y
| until [] = []

fun lift (x::y) = if x = #"e" orelse x = #"E" then [] else x::lift(y)
| lift [] = []

fun uptoExp (x::y) = if x = #"e" orelse x = #"E" then y else uptoExp(y)
| uptoExp [] = []

fun subIntInt s = (NUM(getOpt(Int.fromString(implode(extract(tl(explode(s))))), 0)), NUM(getOpt(Int.fromString(implode(extract(tl(upto(explode(s)))))), 0)))

fun subIdInt s = (ID(implode(extract(tl(explode(s))))), NUM(getOpt(Int.fromString(implode(extract(tl(upto(explode(s)))))), 0)))

fun subIntId s = (NUM(getOpt(Int.fromString(implode(extract(tl(explode(s))))), 0)), ID(implode(extract(tl(upto(explode(s)))))))

fun subIdId s = (ID(implode(extract(tl(explode(s))))), ID(implode(extract(tl(upto(explode(s)))))))

fun getReal s = (getOpt(Real.fromString(implode(lift(explode(s)))), 0.0), getOpt(Int.fromString(implode(lift(uptoExp(explode(s))))), 0))

%%
%structure MLlex
alpha = [A-Za-z];
digit = [0-9];
ws = [\ \t\r\n];
expr = \"[^"\n]*\";
id = [a-zA-Z][a-zA-Z0-9_\']*;
realno = 0\.0 | -?[1-9]{digit}*\.0 | -?0\.{digit}*[1-9] | -?[1-9]{digit}*\.{digit}*[1-9];
realexpno = {realno}[Ee](0 | -?[1-9]{digit}*);
badno = -?0+{digit}+ | -?0+{digit}+\.{digit}* | -?{digit}*\.{digit}+0+ | -0 | -0.0 | \.{digit}+ | {digit}+\.;
badid = ([0-9]+[a-zA-Z\'_]+) | (_[0-9A-Za-z\'_]+) | (\'[0-9A-Za-z_\']+) | ([0-9]+[a-zA-Z\'_]+[0-9]+[a-zA-Z\'_]*);
%%

"+"      => (PLUS);
"-"      => (SUB);
"*"      => (TIMES);
"/"      => (DIV);
"%"      => (MOD);
"abs"    => (ABS);
"exp"    => (EXP);
"("		 => (LEFTPAREN);
")"    	 => (RIGHTPAREN);
"^"      => (CONCAT);
","		 => (COMMA);
":"		 => (COLON);
">"		 => (GREATER);
">="	 => (GREATEROREQUAL);
"<="	 => (LESSOREQUAL);
"="		 => (EQUAL);
"<>"	 => (NOTEQUAL);
"<"		 => (LESS);
"and"    => (AND);
"or"     => (OR);
"not"    => (NOT);
"let"    => (LET);
"if"     => (IF);
"then"   => (THEN);
"else"   => (ELSE);
"end"    => (END);
{ws}+    => (lex());
\[(0 | [1-9]{digit}*)\.\.(0 | [1-9]{digit}*)\]    => (SUBSTRING (subIntInt yytext));
\[{id}\.\.(0 | [1-9]{digit}*)\]    => (SUBSTRING (subIdInt yytext));
\[(0 | [1-9]{digit}*)\.\.{id}\]    => (SUBSTRING (subIntId yytext));
\[{id}\.\.{id}\]    => (SUBSTRING (subIdId yytext));
\[\.\.\.(0 | [1-9]{digit}*)] => (SUBSTRING(NUM(0), NUM(getOpt(Int.fromString(implode(extract(until(explode(yytext))))), 0))));
\[\.\.\.{id}] => (SUBSTRING(NUM(0), ID(implode(extract(until(explode(yytext)))))));
\[(0 | [1-9]{digit}*)\.\.\.] => (SUBSTRING(NUM(getOpt(Int.fromString(implode(extract(until(explode(yytext))))), 0)), NUM(~1)));
\[{id}\.\.\.] => (SUBSTRING(ID(implode(extract(until(explode(yytext))))), NUM(~1)));
(\[({badno} | {realno} | {realexpno} | -{digit}+)\.\.({digit}+ | {id})\]) | (\[({digit}+ | {id})\.\.({badno} | {realno} | {realexpno} | -{digit}+)\]) | (\[{badid}\.\.(-?{digit}+ | {id})\]) | (\[(-?{digit}+ | {id})\.\.{badid}\]) => ((error ("Invalid substring expression : " ^ yytext)); lex());
\[[a-zA-Z0-9]+\.\.\.+[a-zA-Z0-9]+\] => ((error ("Invalid substring expression : " ^ yytext)); lex());
"true"   => (BOOLEAN true);
"false"  => (BOOLEAN false);
{expr}   => (STRING (substring (yytext,1,((size yytext) - 2))));
{id} => (ID yytext);
{badno} => ((error ("Invalid number/real : " ^ yytext)); lex());
({badno}|(0 | -?[1-9]{digit}*))[Ee]({badno}|{realno}|{digit}+) => ((error ("Invalid number/real : " ^ yytext)); lex());
({realno}[Ee]{realno}) | ({realno}[Ee]{badno}) => ((error ("Invalid number/real : " ^ yytext)); lex());
{badid} => ((error ("Invalid identifier : " ^ yytext)); lex());
{realno} => (REAL (getOpt(Real.fromString(yytext), 0.0), 0));
{realexpno} => (REAL(getReal yytext));
0 | -?[1-9]{digit}* => (NUM (getOpt(Int.fromString(yytext), 0)));
.        => (error ("Ignoring bad character " ^ yytext); lex());