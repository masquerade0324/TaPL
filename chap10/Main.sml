structure Main =
struct
  structure LrVals = SimpleBoolLrValsFun (structure Token = LrParser.Token)
  structure Lex = SimpleBoolLexFun (structure Tokens = LrVals.Tokens)
  structure Parser = Join (structure Lex = Lex
                           structure ParserData = LrVals.ParserData
                           structure LrParser = LrParser)

  fun printErr (s, _, _) = print (s ^ "\n")

  fun parse filename =
    let
      val f = TextIO.openIn filename
      val lexer = Parser.makeLexer (fn n => TextIO.inputN (f, n))
    in
      Parser.parse (0, lexer, printErr, ())
    end

  (* parse, evaluate and print *)
  fun run filename =
    let
      val ((ctx, term), _) = parse filename
      val typ = Evaluator.typeof ctx term
      val nf = Evaluator.eval ctx term
    in
      print "Type:\n";
      print ("\t" ^ Syntax.tyToStr typ ^ "\n");
      print "Term Before Eval:\n";
      print ("\t" ^ Syntax.termToStr ctx term ^ "\n");
      print "Term After Eval:\n";
      print ("\t" ^ Syntax.termToStr ctx nf ^ "\n")
    end
end
