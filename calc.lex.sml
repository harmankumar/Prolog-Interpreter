functor CalcLexFun(structure Tokens: Calc_TOKENS)  = struct

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
fun eof () = Tokens.EOF(!pos,!pos)
	
fun error (e,l,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"])


fun inc x = x:=(!x)+1 ; 



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
		      val finals = map (fn i => Vector.sub (actTable, i)) finals'
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
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (inc(pos);lex()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IF(!pos , !pos)))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DOT(!pos , !pos)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(!pos, !pos)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(!pos, !pos)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(!pos, !pos)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMI(!pos,!pos)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LSOLIDPAREN(!pos,!pos)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RSOLIDPAREN(!pos,!pos)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.CONS(!pos,!pos)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PLUS(!pos, !pos)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IMPOS(!pos,!pos)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SUB(!pos, !pos)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MULT(!pos, !pos)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIV(!pos, !pos)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.QUERY(!pos,!pos)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MOD(!pos, !pos)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ABS(!pos, !pos)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EXP(!pos, !pos)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT(!pos, !pos)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT(!pos, !pos)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQ(!pos, !pos)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEQ(!pos, !pos)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GTEQ(!pos, !pos)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LTEQ(!pos, !pos)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BOOLEAN( true , !pos , !pos)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BOOLEAN( false , !pos , !pos)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BOOLEAN( true , !pos , !pos)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BOOLEAN( false , !pos , !pos)))
fun yyAction30 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.NUM(Option.valOf( Int.fromString(yytext)) , !pos , !pos ) )
      end
fun yyAction31 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (error(" Invalid number "^yytext , !pos , !pos);lex())
      end
fun yyAction32 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.RATIONAL( (Option.valOf(Real.fromString(yytext))) , !pos, !pos) )
      end
fun yyAction33 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.RATIONAL( (Option.valOf(Real.fromString(yytext))) , !pos, !pos) )
      end
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND(!pos,!pos)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(!pos,!pos)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NOT(!pos,!pos)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FUNC(!pos,!pos)))
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.UCID( yytext , !pos , !pos))
      end
fun yyAction39 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.LCID( yytext , !pos , !pos))
      end
fun yyAction40 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (error(" Illegal character "^yytext , !pos , !pos );lex())
      end
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ35(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                      else yyAction26(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction26(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction26(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ39(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"u"
                  then yyQ38(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ37(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction36(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                      else yyAction36(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction36(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction36(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction36(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ36(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ41(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ40(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction17(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"d"
                  then yyQ43(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ42(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                      else yyAction27(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction27(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction27(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ36(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ47(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ46(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ45(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ44(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                      else yyAction18(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ49(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction39(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                      else yyAction39(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction39(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction39(strm, yyNO_MATCH)
                  else yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp = #"a"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = #"c"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp < #"c"
              then yyQ48(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ36(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ50(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                      else yyAction28(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ50(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction28(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction28(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ50(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ53(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"u"
                  then yyQ52(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ51(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ50(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                      else yyAction37(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ50(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ50(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ57(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"u"
                  then yyQ56(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"g"
              then if inp = #"f"
                  then yyQ55(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"z"
              then if inp = #"y"
                  then yyQ54(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ50(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                      else yyAction29(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction29(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ50(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction29(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction29(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ50(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
                  else yyAction29(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ61(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ60(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"a"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ59(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction38(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"a"
              then yyQ58(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ62(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ64(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ63(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ65(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ72(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
            else if inp < #"0"
              then yyAction33(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ72(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"1"
              then yyQ72(strm', lastMatch)
            else if inp < #"1"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ72(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ71(strm', lastMatch)
            else if inp < #"0"
              then if inp = #"-"
                  then yyQ70(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ72(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"E"
              then yyQ69(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
            else if inp < #"E"
              then if inp = #"0"
                  then yyQ68(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction32(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ68(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
                  else yyAction32(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ69(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ68(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ68(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction30(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ66(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyAction30(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ67(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction30(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ66(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyAction30(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ67(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction31(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction30(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"/"
                  then yyAction30(strm, yyNO_MATCH)
                else if inp < #"/"
                  then if inp = #"."
                      then yyQ66(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                      else yyAction30(strm, yyNO_MATCH)
                  else yyQ73(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp = #"["
              then yyAction30(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction30(strm, yyNO_MATCH)
                  else yyQ73(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp = #"a"
              then yyQ73(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"a"
              then yyAction30(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ73(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"1"
                  then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ76(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ73(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"1"
                  then yyQ73(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ76(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                      else yyAction30(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ73(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyAction30(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ73(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ73(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
                  else yyAction30(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ73(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"1"
                  then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ76(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                      else yyAction31(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
                  else yyAction31(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ73(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yystuck(lastMatch)
            else if inp < #"."
              then if inp = #"-"
                  then yyQ74(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ77(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ75(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"-"
                  then yyQ74(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ67(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ78(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ79(strm', yyMATCH(strm, yyAction40, yyNO_MATCH))
              else yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ80(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
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
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"?"
              then yyQ21(strm', lastMatch)
            else if inp < #"?"
              then if inp = #"*"
                  then yyQ8(strm', lastMatch)
                else if inp < #"*"
                  then if inp = #"$"
                      then yyQ4(strm', lastMatch)
                    else if inp < #"$"
                      then if inp = #"\v"
                          then yyQ1(strm', lastMatch)
                        else if inp < #"\v"
                          then if inp = #"\t"
                              then yyQ2(strm', lastMatch)
                            else if inp = #"\n"
                              then yyQ3(strm', lastMatch)
                              else yyQ1(strm', lastMatch)
                        else if inp = #" "
                          then yyQ2(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = #"'"
                      then yyQ1(strm', lastMatch)
                    else if inp < #"'"
                      then if inp = #"%"
                          then yyQ1(strm', lastMatch)
                          else yyQ5(strm', lastMatch)
                    else if inp = #"("
                      then yyQ6(strm', lastMatch)
                      else yyQ7(strm', lastMatch)
                else if inp = #"1"
                  then yyQ15(strm', lastMatch)
                else if inp < #"1"
                  then if inp = #"."
                      then yyQ12(strm', lastMatch)
                    else if inp < #"."
                      then if inp = #","
                          then yyQ10(strm', lastMatch)
                        else if inp = #"+"
                          then yyQ9(strm', lastMatch)
                          else yyQ11(strm', lastMatch)
                    else if inp = #"/"
                      then yyQ13(strm', lastMatch)
                      else yyQ14(strm', lastMatch)
                else if inp = #"<"
                  then yyQ18(strm', lastMatch)
                else if inp < #"<"
                  then if inp = #":"
                      then yyQ16(strm', lastMatch)
                    else if inp = #";"
                      then yyQ17(strm', lastMatch)
                      else yyQ15(strm', lastMatch)
                else if inp = #"="
                  then yyQ19(strm', lastMatch)
                  else yyQ20(strm', lastMatch)
            else if inp = #"a"
              then yyQ28(strm', lastMatch)
            else if inp < #"a"
              then if inp = #"T"
                  then yyQ25(strm', lastMatch)
                else if inp < #"T"
                  then if inp = #"G"
                      then yyQ22(strm', lastMatch)
                    else if inp < #"G"
                      then if inp = #"A"
                          then yyQ22(strm', lastMatch)
                        else if inp < #"A"
                          then yyQ1(strm', lastMatch)
                        else if inp = #"F"
                          then yyQ23(strm', lastMatch)
                          else yyQ22(strm', lastMatch)
                    else if inp = #"M"
                      then yyQ24(strm', lastMatch)
                      else yyQ22(strm', lastMatch)
                else if inp = #"\\"
                  then yyQ1(strm', lastMatch)
                else if inp < #"\\"
                  then if inp = #"["
                      then yyQ26(strm', lastMatch)
                      else yyQ22(strm', lastMatch)
                else if inp = #"]"
                  then yyQ27(strm', lastMatch)
                  else yyQ1(strm', lastMatch)
            else if inp = #"o"
              then yyQ29(strm', lastMatch)
            else if inp < #"o"
              then if inp = #"g"
                  then yyQ29(strm', lastMatch)
                else if inp < #"g"
                  then if inp = #"f"
                      then yyQ30(strm', lastMatch)
                      else yyQ29(strm', lastMatch)
                else if inp = #"m"
                  then yyQ31(strm', lastMatch)
                else if inp = #"n"
                  then yyQ32(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"{"
              then yyQ1(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"t"
                  then yyQ33(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
            else if inp = #"|"
              then yyQ34(strm', lastMatch)
              else yyQ1(strm', lastMatch)
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
