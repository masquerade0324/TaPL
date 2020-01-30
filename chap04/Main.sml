structure Main =
struct
  structure LrVals = ArithLrValsFun (structure Token = LrParser.Token)
  structure Lex = ArithLexFun (structure Tokens = LrVals.Tokens)
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
      val (term, _) = parse filename
    in
      print (Syntax.toString (Evaluator.eval term) ^ "\n")
    end
end
