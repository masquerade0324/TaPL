functor ArithLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Arith_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure S = Syntax


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\012\000\004\000\011\000\005\000\010\000\006\000\009\000\
\\007\000\008\000\008\000\007\000\009\000\006\000\010\000\005\000\000\000\
\\001\000\002\000\024\000\003\000\024\000\011\000\024\000\012\000\024\000\000\000\
\\001\000\002\000\025\000\003\000\025\000\011\000\025\000\012\000\025\000\000\000\
\\001\000\002\000\026\000\003\000\026\000\011\000\026\000\012\000\026\000\000\000\
\\001\000\002\000\027\000\003\000\027\000\011\000\027\000\012\000\027\000\000\000\
\\001\000\002\000\028\000\003\000\028\000\011\000\028\000\012\000\028\000\000\000\
\\001\000\002\000\029\000\003\000\029\000\011\000\029\000\012\000\029\000\000\000\
\\001\000\002\000\030\000\003\000\030\000\011\000\030\000\012\000\030\000\000\000\
\\001\000\002\000\031\000\003\000\031\000\011\000\031\000\012\000\031\000\000\000\
\\001\000\002\000\032\000\003\000\032\000\011\000\032\000\012\000\032\000\000\000\
\\001\000\002\000\033\000\003\000\033\000\011\000\033\000\012\000\033\000\000\000\
\\001\000\002\000\019\000\000\000\
\\001\000\003\000\021\000\000\000\
\\001\000\004\000\011\000\005\000\010\000\009\000\006\000\010\000\005\000\000\000\
\\001\000\011\000\018\000\000\000\
\\001\000\012\000\000\000\000\000\
\"
val actionRowNumbers =
"\000\000\003\000\001\000\000\000\
\\010\000\013\000\013\000\013\000\
\\009\000\008\000\000\000\014\000\
\\006\000\005\000\004\000\011\000\
\\007\000\000\000\012\000\000\000\
\\002\000\015\000"
val gotoT =
"\
\\001\000\021\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\011\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\003\000\012\000\000\000\
\\003\000\013\000\000\000\
\\003\000\014\000\000\000\
\\000\000\
\\000\000\
\\001\000\015\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\018\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\001\000\020\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 22
val numrules = 10
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | atterm of unit ->  (S.term) | appterm of unit ->  (S.term)
 | term of unit ->  (S.term)
end
type svalue = MlyValue.svalue
type result = S.term
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 11) => true | _ => false
val showTerminal =
fn (T 0) => "IF"
  | (T 1) => "THEN"
  | (T 2) => "ELSE"
  | (T 3) => "TRUE"
  | (T 4) => "FALSE"
  | (T 5) => "SUCC"
  | (T 6) => "PRED"
  | (T 7) => "ISZERO"
  | (T 8) => "ZERO"
  | (T 9) => "LPAREN"
  | (T 10) => "RPAREN"
  | (T 11) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.appterm appterm1, appterm1left, 
appterm1right)) :: rest671)) => let val  result = MlyValue.term (fn _
 => let val  (appterm as appterm1) = appterm1 ()
 in (appterm)
end)
 in ( LrTable.NT 0, ( result, appterm1left, appterm1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.term term3, _, term3right)) :: _ :: ( _, ( 
MlyValue.term term2, _, _)) :: _ :: ( _, ( MlyValue.term term1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.term (fn _ => let val  term1 = term1 ()
 val  term2 = term2 ()
 val  term3 = term3 ()
 in (S.TmIf (term1, term2, term3))
end)
 in ( LrTable.NT 0, ( result, IF1left, term3right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.atterm atterm1, atterm1left, atterm1right))
 :: rest671)) => let val  result = MlyValue.appterm (fn _ => let val 
 (atterm as atterm1) = atterm1 ()
 in (atterm)
end)
 in ( LrTable.NT 1, ( result, atterm1left, atterm1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.atterm atterm1, _, atterm1right)) :: ( _, (
 _, SUCC1left, _)) :: rest671)) => let val  result = MlyValue.appterm
 (fn _ => let val  (atterm as atterm1) = atterm1 ()
 in (S.TmSucc atterm)
end)
 in ( LrTable.NT 1, ( result, SUCC1left, atterm1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.atterm atterm1, _, atterm1right)) :: ( _, (
 _, PRED1left, _)) :: rest671)) => let val  result = MlyValue.appterm
 (fn _ => let val  (atterm as atterm1) = atterm1 ()
 in (S.TmPred atterm)
end)
 in ( LrTable.NT 1, ( result, PRED1left, atterm1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.atterm atterm1, _, atterm1right)) :: ( _, (
 _, ISZERO1left, _)) :: rest671)) => let val  result = 
MlyValue.appterm (fn _ => let val  (atterm as atterm1) = atterm1 ()
 in (S.TmIsZero atterm)
end)
 in ( LrTable.NT 1, ( result, ISZERO1left, atterm1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.term term1, _
, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.atterm (fn _ => let val  (term as term1) = term1 ()
 in (term)
end)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 7, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.atterm (fn _ => (S.TmTrue))
 in ( LrTable.NT 2, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 8, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val 
 result = MlyValue.atterm (fn _ => (S.TmFalse))
 in ( LrTable.NT 2, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 9, ( ( _, ( _, ZERO1left, ZERO1right)) :: rest671)) => let val  
result = MlyValue.atterm (fn _ => (S.TmZero))
 in ( LrTable.NT 2, ( result, ZERO1left, ZERO1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.term x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Arith_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun SUCC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun PRED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun ISZERO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ZERO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
end
end
