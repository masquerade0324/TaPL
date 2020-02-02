functor SimpleBoolLexFun (structure Tokens: SimpleBool_TOKENS)  = struct

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

structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token

fun error s = print (s ^ "\n")
fun eof ()  = T.EOF (0, 0)



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
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (lex ()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (lex ()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "bool\n";
                      T.BOOL (0, 0)))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "else\n";
                      T.ELSE (0, 0)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "false\n";
                      T.FALSE (0, 0)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "fn\n";
                      T.FN (0, 0)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "if\n";
                      T.IF (0, 0)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "iszero\n";
                      T.ISZERO (0, 0)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "pred\n";
                      T.PRED (0, 0)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "succ\n";
                      T.SUCC (0, 0)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "then\n";
                      T.THEN (0, 0)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "true\n";
                      T.TRUE (0, 0)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "val\n";
                      T.VAL (0, 0)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "0\n";
                      T.ZERO (0, 0)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "(\n";
                      T.LPAREN (0, 0)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print ")\n";
                      T.RPAREN (0, 0)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "->\n";
                      T.ARROW (0, 0)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print "=>\n";
                      T.DARROW (0, 0)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print ":\n";
                      T.COLON (0, 0)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (print ";\n";
                      T.SEMI (0, 0)))
fun yyAction20 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print ("ID (" ^ yytext ^ ")\n");
                  T.ID (yytext, 0, 0))
      end
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm; (lex ()))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"["
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"a"
              then yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                      else yyAction12(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = #"["
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction12(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"a"
              then yyAction12(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ22(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ21(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                      else yyAction11(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction11(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = #"["
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction11(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < #"a"
              then yyAction11(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ26(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ25(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else yyAction10(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction10(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = #"["
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction10(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"a"
              then yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ28(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ27(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"i"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"a"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp = #"h"
                  then yyQ23(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"s"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ24(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                      else yyAction9(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction9(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp = #"["
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction9(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < #"a"
              then yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"c"
              then yyQ31(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"c"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"c"
              then yyQ30(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"c"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ29(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyAction8(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"["
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction8(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"a"
              then yyAction8(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ34(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ33(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ32(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                      else yyAction7(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = #"["
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"a"
              then yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ40(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ39(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ38(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"z"
              then yyQ37(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"y"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                      else yyAction6(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"["
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < #"a"
              then yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"g"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"g"
              then if inp = #"a"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp = #"f"
                  then yyQ35(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"t"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ36(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                      else yyAction5(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction5(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp = #"["
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction5(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"a"
              then yyAction5(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                      else yyAction4(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"["
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"a"
              then yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ45(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ44(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ43(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"n"
              then yyQ42(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"a"
                  then yyQ41(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                      else yyAction3(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"["
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"a"
              then yyAction3(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ48(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ47(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ46(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"["
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"a"
              then yyAction2(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ51(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ50(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ49(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp <= #"`"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #":"
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #":"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp <= #"/"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"["
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction20(strm, yyNO_MATCH)
                  else yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp = #"a"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"a"
              then yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ20(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ52(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ53(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ54(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ54(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ54(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ54(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ11(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"-"
                  then yyQ6(strm', lastMatch)
                else if inp < #"-"
                  then if inp = #" "
                      then yyQ2(strm', lastMatch)
                    else if inp < #" "
                      then if inp = #"\n"
                          then yyQ3(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ2(strm', lastMatch)
                              else yyQ1(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = #")"
                      then yyQ5(strm', lastMatch)
                    else if inp < #")"
                      then if inp = #"("
                          then yyQ4(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = #";"
                  then yyQ9(strm', lastMatch)
                else if inp < #";"
                  then if inp = #"1"
                      then yyQ1(strm', lastMatch)
                    else if inp < #"1"
                      then if inp = #"0"
                          then yyQ7(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = #":"
                      then yyQ8(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = #"="
                  then yyQ10(strm', lastMatch)
                  else yyQ1(strm', lastMatch)
            else if inp = #"j"
              then yyQ11(strm', lastMatch)
            else if inp < #"j"
              then if inp = #"c"
                  then yyQ11(strm', lastMatch)
                else if inp < #"c"
                  then if inp = #"a"
                      then yyQ11(strm', lastMatch)
                    else if inp < #"a"
                      then if inp <= #"Z"
                          then yyQ11(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                      else yyQ12(strm', lastMatch)
                else if inp = #"f"
                  then yyQ14(strm', lastMatch)
                else if inp < #"f"
                  then if inp = #"e"
                      then yyQ13(strm', lastMatch)
                      else yyQ11(strm', lastMatch)
                else if inp = #"i"
                  then yyQ15(strm', lastMatch)
                  else yyQ11(strm', lastMatch)
            else if inp = #"t"
              then yyQ18(strm', lastMatch)
            else if inp < #"t"
              then if inp = #"q"
                  then yyQ11(strm', lastMatch)
                else if inp < #"q"
                  then if inp = #"p"
                      then yyQ16(strm', lastMatch)
                      else yyQ11(strm', lastMatch)
                else if inp = #"s"
                  then yyQ17(strm', lastMatch)
                  else yyQ11(strm', lastMatch)
            else if inp = #"w"
              then yyQ11(strm', lastMatch)
            else if inp < #"w"
              then if inp = #"u"
                  then yyQ11(strm', lastMatch)
                  else yyQ19(strm', lastMatch)
            else if inp <= #"z"
              then yyQ11(strm', lastMatch)
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
    end

  end
