fun print_error (s,i,_) =
  TextIO.output(TextIO.stdOut,
    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

structure Fol = struct

  structure FolLrVals =
    FolLrValsFun(structure Token = LrParser.Token)

  structure FolLex =
    FolLexFun(structure Tokens = FolLrVals.Tokens)

  structure FolParser =
    Join(structure LrParser = LrParser
	 structure ParserData = FolLrVals.ParserData
	 structure Lex = FolLex)

  fun invoke lexstream =
       FolParser.parse(0,lexstream,print_error,())

  fun parse_file f = let 
	val file = TextIO.openIn f
	val s = TextIO.input file
	val lexer = FolParser.makeLexer (fn _ => s)
	val dummySEMI = FolLrVals.Tokens.SEMI(0,0)
	val (result,lexer) = invoke lexer
  in
    result before TextIO.closeIn file
  end
  
  fun parse_string s = let 
	val lexer = FolParser.makeLexer (fn _ => s)
    val dummySEMI = FolLrVals.Tokens.SEMI(0,0)
    val (result,lexer) = invoke lexer
	
    in result
  end
end