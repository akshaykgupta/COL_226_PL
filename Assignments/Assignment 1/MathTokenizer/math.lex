datatype lexresult = PLUS | SUB | TIMES | DIV | ID of string | NUM of int | EOF

val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = fn () => EOF

%%
%structure MathLex
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t\n];
%%
"+"      => (PLUS);
"-"      => (SUB);
"*"      => (TIMES);
"/"      => (DIV);
{ws}+    => (lex());
{alpha}+ => (ID yytext);
{digit}+ => (NUM (foldl (fn(a,r)=>ord(a)-ord(#"0")+10*r) 0 (explode yytext)));
.        => (error ("MathLex: ignoring bad character " ^ yytext); lex());