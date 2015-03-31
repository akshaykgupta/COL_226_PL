open TextIO;
use "math.lex.sml";
open MathLex;
open UserDeclarations;
val lexer = makeLexer( fn n => valOf(inputLine( stdIn ) ));