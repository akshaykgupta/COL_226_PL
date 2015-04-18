functor FolLexFun(structure Tokens: Fol_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

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



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (pos := (!pos) + 1; lex()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS(!pos, !pos)))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SUB(!pos, !pos)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TIMES(!pos, !pos)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIV(!pos, !pos)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MOD(!pos, !pos)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BY(!pos, !pos)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ABS(!pos, !pos)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EXP(!pos, !pos)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LEFTPAREN(!pos, !pos)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RIGHTPAREN(!pos, !pos)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(!pos, !pos)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GREATER(!pos, !pos)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GREATEROREQUAL(!pos, !pos)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LESSOREQUAL(!pos, !pos)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQUAL(!pos, !pos)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NOTEQUAL(!pos, !pos)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LESS(!pos, !pos)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND(!pos, !pos)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(!pos, !pos)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NOT(!pos, !pos)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT(!pos,!pos)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.QUERY(!pos, !pos)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.UNDERSCORE(!pos, !pos)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.CONS(!pos, !pos)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.STARTLIST(!pos, !pos)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ENDLIST(!pos, !pos)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMI(!pos,!pos)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IF(!pos, !pos)))
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.BOOL(valOf(Bool.fromString yytext),!pos,!pos))
      end
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.BOOL(valOf(Bool.fromString yytext),!pos,!pos))
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.UCID (yytext,!pos,!pos))
      end
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LCID (yytext,!pos,!pos))
      end
fun yyAction34 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.NUM (valOf(Int.fromString yytext),!pos,!pos))
      end
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error ("Invalid number/real : " ^ yytext, !pos, !pos); lex())
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error ("Invalid number/real : " ^ yytext, !pos, !pos); lex())
      end
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error ("Invalid number/real : " ^ yytext, !pos, !pos); lex())
      end
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.REAL((valOf(Real.fromString yytext), 0),!pos,!pos))
      end
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.REAL(getReal yytext,!pos,!pos))
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error ("Ignoring illegal character : " ^ yytext, !pos, !pos); lex())
      end
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ51(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ51(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
and yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ51(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ52(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ51(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ51(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ51(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ50(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ49(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ57(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ58(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ57(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
and yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ57(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ58(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ57(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ57(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"0"
              then yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ57(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ56(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ55(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ56(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ55(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ51(strm', lastMatch)
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ52(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ51(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ51(strm', lastMatch)
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ59(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ51(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ54(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"."
                  then yyQ53(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ55(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ60(strm', lastMatch)
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ61(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ60(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ60(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ61(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ60(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ60(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ60(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ48(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"."
                  then yyQ47(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ49(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ51(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"0"
              then yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ51(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ62(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ45(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ57(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"0"
              then yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ57(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ65(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ64(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ63(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ65(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ64(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ63(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ62(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ64(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ57(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ57(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ68(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction37(strm, yyNO_MATCH)
              else yyQ67(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ66(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ44(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"."
                  then yyQ43(strm', lastMatch)
                else if inp < #"."
                  then if inp = #"-"
                      then yyQ42(strm', lastMatch)
                      else yystuck(lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"~"
              then yyQ46(strm', lastMatch)
            else if inp < #"~"
              then if inp <= #"9"
                  then yyQ45(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ80(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ81(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ80(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
and yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ80(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ81(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ80(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ80(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ80(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yystuck(lastMatch)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ79(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ78(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ86(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ87(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ86(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
and yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ86(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ87(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ86(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ86(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"0"
              then yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ86(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction36(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ85(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ84(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ83(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ85(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ84(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ80(strm', lastMatch)
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ81(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ80(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ80(strm', lastMatch)
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ88(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ80(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ83(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"."
                  then yyQ82(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ84(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ89(strm', lastMatch)
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ90(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ89(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ89(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ90(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ89(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ89(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ89(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ77(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"."
                  then yyQ76(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ78(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ80(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"0"
              then yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ80(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction36(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ91(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ74(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ86(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"0"
              then yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ86(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction36(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ94(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ93(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ92(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ94(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ93(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ92(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ91(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ93(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ86(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ86(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ97(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction36(strm, yyNO_MATCH)
              else yyQ96(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ95(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ73(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"."
                  then yyQ72(strm', lastMatch)
                else if inp < #"."
                  then if inp = #"-"
                      then yyQ71(strm', lastMatch)
                      else yystuck(lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"~"
              then yyQ75(strm', lastMatch)
            else if inp < #"~"
              then if inp <= #"9"
                  then yyQ74(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"1"
                  then yyQ69(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ40(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                      else yyAction35(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ69(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
and yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ41(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"1"
                  then yyQ69(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ40(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ69(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ41(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ41(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"1"
                  then yyQ39(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ40(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ39(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ41(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ39(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ39(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ38(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ37(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"1"
                  then yyQ102(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ103(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                      else yyAction35(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ102(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
and yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"1"
                  then yyQ102(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ103(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                      else yyAction35(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ102(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"0"
                  then yyQ102(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction35(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ102(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction35(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ101(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                      else yyAction35(strm, yyNO_MATCH)
                  else yyQ100(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp = #"F"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction35(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ101(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                      else yyAction35(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ99(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyQ100(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp = #"F"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ69(strm', lastMatch)
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ40(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ69(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ69(strm', lastMatch)
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ104(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ69(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ99(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"."
                  then yyQ98(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ100(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ105(strm', lastMatch)
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ106(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"1"
                  then yyQ105(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ106(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                      else yyAction35(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ105(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ105(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ105(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ36(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"."
                  then yyQ35(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
                  else yyAction40(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ37(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ107(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction30(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                      else yyAction30(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction30(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ107(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyAction30(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction30(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction30(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ107(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyAction30(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ110(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"u"
                  then yyQ109(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ108(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ107(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ107(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ107(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ111(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ107(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                      else yyAction21(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ107(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ107(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ113(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ112(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ107(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                      else yyAction6(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ107(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ107(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"d"
                  then yyQ115(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ114(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ107(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ107(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction31(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction31(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ107(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ119(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ118(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ117(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ116(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ107(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                      else yyAction9(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction9(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ107(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ107(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"q"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"q"
              then if inp = #"p"
                  then yyQ121(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"y"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"y"
              then if inp = #"x"
                  then yyQ120(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ107(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                      else yyAction5(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ107(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ107(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ122 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"w"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"w"
              then if inp = #"v"
                  then yyQ123(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ122(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ126 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ107(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ107(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ107(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ125 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"d"
                  then yyQ126(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ127 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ107(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyAction8(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction8(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ107(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ107(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ124 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction33(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"a"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ127(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                      else yyAction33(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = #"c"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"c"
              then if inp = #"a"
                  then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                else if inp = #"`"
                  then yyAction33(strm, yyNO_MATCH)
                  else yyQ124(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp = #"o"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ125(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ107(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ128 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction32(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                      else yyAction32(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction32(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                  else yyAction32(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction32(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction32(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                  else yyAction32(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction32(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                      else yyAction32(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction32(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                  else yyAction32(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction32(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction32(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                  else yyAction32(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ128(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ129 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ129(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ131 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ130 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ131(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ130(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ132 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ132(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ133 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"0"
                  then yyQ39(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction35(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ39(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ134 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction34(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ133(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                      else yyAction34(strm, yyNO_MATCH)
                  else yyQ134(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp = #"F"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ70(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction34(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ133(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                      else yyAction34(strm, yyNO_MATCH)
                  else yyQ134(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp = #"F"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ70(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ137 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"0"
                  then yyQ102(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction35(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ102(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ136 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction35(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ137(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                      else yyAction35(strm, yyNO_MATCH)
                  else yyQ136(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp = #"F"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ135 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction35(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ137(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                      else yyAction35(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ135(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyQ136(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp = #"F"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction34(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ133(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                      else yyAction34(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ135(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyQ136(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp = #"F"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ70(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ102(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp < #"0"
              then yyAction22(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ102(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ139 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ70(strm', lastMatch)
            else if inp < #"E"
              then if inp = #"0"
                  then yyQ139(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ139(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"e"
              then yyQ70(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ142 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction35(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ91(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                      else yyAction35(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ92(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyQ93(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp = #"F"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ141 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ142(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"."
                  then yyQ72(strm', lastMatch)
                else if inp < #"."
                  then if inp = #"-"
                      then yyQ71(strm', lastMatch)
                      else yystuck(lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"~"
              then yyQ75(strm', lastMatch)
            else if inp < #"~"
              then if inp <= #"9"
                  then yyQ74(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ143 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"F"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"F"
              then if inp = #"E"
                  then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ140 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ143(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ138 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ141(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"\n"
                  then yyAction35(strm, yyNO_MATCH)
                  else yyQ140(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp = #"e"
              then yyQ141(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyQ140(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ139(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"1"
              then if inp = #"0"
                  then yyQ138(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ139(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ144 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ144(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ144(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ144(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ144(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"-"
                  then yyQ9(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"-"
                  then if inp = #"!"
                      then yyQ1(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #"!"
                      then if inp = #"\n"
                          then yyQ3(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ2(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                              else yyQ1(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp = #" "
                          then yyQ2(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ1(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"*"
                      then yyQ6(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #"*"
                      then if inp = #"("
                          then yyQ4(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp = #")"
                          then yyQ5(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ1(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"+"
                      then yyQ7(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ8(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #";"
                  then yyQ15(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #";"
                  then if inp = #"0"
                      then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #"0"
                      then if inp = #"."
                          then yyQ10(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ11(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #":"
                      then yyQ14(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ13(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #">"
                  then yyQ18(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #">"
                  then if inp = #"<"
                      then yyQ16(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ17(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"?"
                  then yyQ19(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ1(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"g"
              then yyQ25(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"g"
              then if inp = #"`"
                  then yyQ1(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"`"
                  then if inp = #"]"
                      then yyQ22(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp < #"]"
                      then if inp = #"["
                          then yyQ21(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                        else if inp = #"\\"
                          then yyQ1(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                          else yyQ20(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"^"
                      then yyQ1(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ23(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"d"
                  then yyQ26(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"d"
                  then if inp = #"a"
                      then yyQ24(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ25(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"e"
                  then yyQ27(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ28(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"u"
              then yyQ25(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"o"
                  then yyQ31(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"o"
                  then if inp = #"m"
                      then yyQ29(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                    else if inp = #"n"
                      then yyQ30(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyQ25(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"t"
                  then yyQ32(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ25(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"}"
              then yyQ1(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"}"
              then if inp = #"{"
                  then yyQ1(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp = #"|"
                  then yyQ33(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyQ25(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"~"
              then yyQ34(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyQ1(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
