structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token

fun error s = print (s ^ "\n")
fun eof ()  = T.EOF (0, 0)

%%
%full
%header (functor SimpleBoolLexFun (structure Tokens: SimpleBool_TOKENS));

alpha = [a-zA-Z];
digit = [0-9];
id    = {alpha}({alpha}|{digit}|"'")*;
ws    = [\ \t];
eol   = "\n";

%%
<INITIAL>{ws}+ => (lex ());
<INITIAL>{eol} => (lex ());

<INITIAL>"bool"   => (print "bool\n";
                      T.BOOL (0, 0));
<INITIAL>"else"   => (print "else\n";
                      T.ELSE (0, 0));
<INITIAL>"false"  => (print "false\n";
                      T.FALSE (0, 0));
<INITIAL>"fn"     => (print "fn\n";
                      T.FN (0, 0));
<INITIAL>"if"     => (print "if\n";
                      T.IF (0, 0));
<INITIAL>"iszero" => (print "iszero\n";
                      T.ISZERO (0, 0));
<INITIAL>"pred"   => (print "pred\n";
                      T.PRED (0, 0));
<INITIAL>"succ"   => (print "succ\n";
                      T.SUCC (0, 0));
<INITIAL>"then"   => (print "then\n";
                      T.THEN (0, 0));
<INITIAL>"true"   => (print "true\n";
                      T.TRUE (0, 0));
<INITIAL>"val"    => (print "val\n";
                      T.VAL (0, 0));
<INITIAL>"0"      => (print "0\n";
                      T.ZERO (0, 0));
<INITIAL>"("      => (print "(\n";
                      T.LPAREN (0, 0));
<INITIAL>")"      => (print ")\n";
                      T.RPAREN (0, 0));
<INITIAL>"->"     => (print "->\n";
                      T.ARROW (0, 0));
<INITIAL>"=>"     => (print "=>\n";
                      T.DARROW (0, 0));
<INITIAL>":"      => (print ":\n";
                      T.COLON (0, 0));
<INITIAL>";"      => (print ";\n";
                      T.SEMI (0, 0));

<INITIAL>{id} => (print ("ID (" ^ yytext ^ ")\n");
                  T.ID (yytext, 0, 0));

<INITIAL>. => (lex ());
