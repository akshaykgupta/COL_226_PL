open TextIO;
use "Tokenizer.lex.sml";
open MLlex;
open UserDeclarations;

fun Tokenize(fileName) = 
let
	val fin = TextIO.openIn fileName;
	val fileLexer = makeLexer( fn n => getOpt( inputLine(fin), ""));
	fun insertIntoList (tokenList, EOF) = rev (tokenList)
	| insertIntoList (tokenList, s) = insertIntoList(s::tokenList, fileLexer());
in
	insertIntoList([], fileLexer())
end;