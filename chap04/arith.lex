structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token

fun error s = print (s ^ "\n")
fun eof ()  = T.EOF (0, 0)

%%
%full
%header (functor ArithLexFun (structure Tokens: Arith_TOKENS));

ws  = [\ \t];
eol = "\n";

%%
<INITIAL>{ws}+ => (lex ());
<INITIAL>{eol} => (lex ());

<INITIAL>"if"     => (T.IF (0, 0));
<INITIAL>"then"   => (T.THEN (0, 0));
<INITIAL>"else"   => (T.ELSE (0, 0));
<INITIAL>"true"   => (T.TRUE (0, 0));
<INITIAL>"false"  => (T.FALSE (0, 0));
<INITIAL>"succ"   => (T.SUCC (0, 0));
<INITIAL>"pred"   => (T.PRED (0, 0));
<INITIAL>"iszero" => (T.ISZERO (0, 0));
<INITIAL>"0"      => (T.ZERO (0, 0));
<INITIAL>"("      => (T.LPAREN (0, 0));
<INITIAL>")"      => (T.RPAREN (0, 0));

<INITIAL>. => (lex ());
