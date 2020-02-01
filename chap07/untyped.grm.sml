functor UntypedLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Untyped_TOKENS
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
\\001\000\010\000\028\000\011\000\028\000\012\000\028\000\017\000\028\000\000\000\
\\001\000\010\000\029\000\011\000\029\000\012\000\029\000\017\000\029\000\000\000\
\\001\000\010\000\030\000\011\000\030\000\012\000\030\000\017\000\030\000\000\000\
\\001\000\010\000\031\000\011\000\031\000\012\000\031\000\017\000\031\000\000\000\
\\001\000\010\000\009\000\011\000\026\000\012\000\026\000\017\000\006\000\000\000\
\\001\000\010\000\009\000\013\000\008\000\014\000\007\000\017\000\006\000\000\000\
\\001\000\010\000\009\000\014\000\007\000\017\000\006\000\000\000\
\\001\000\011\000\027\000\012\000\027\000\000\000\
\\001\000\011\000\018\000\000\000\
\\001\000\012\000\000\000\000\000\
\\001\000\012\000\022\000\000\000\
\\001\000\012\000\023\000\000\000\
\\001\000\015\000\015\000\000\000\
\\001\000\016\000\024\000\017\000\013\000\000\000\
\\001\000\016\000\025\000\000\000\
\\001\000\016\000\016\000\000\000\
\\001\000\017\000\011\000\000\000\
\\001\000\017\000\013\000\000\000\
\"
val actionRowNumbers =
"\005\000\000\000\004\000\010\000\
\\003\000\016\000\017\000\006\000\
\\001\000\012\000\015\000\013\000\
\\008\000\006\000\006\000\014\000\
\\002\000\007\000\011\000\009\000"
val gotoT =
"\
\\001\000\019\000\003\000\003\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\005\000\008\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\010\000\000\000\
\\003\000\012\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\015\000\000\000\
\\000\000\
\\003\000\017\000\004\000\002\000\005\000\001\000\000\000\
\\003\000\018\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 20
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
 | ID of unit ->  (string) | atterm of unit ->  (S.context -> S.term)
 | appterm of unit ->  (S.context -> S.term)
 | term of unit ->  (S.context -> S.term)
 | binds of unit ->  (S.context) | top of unit ->  (S.context*S.term)
end
type svalue = MlyValue.svalue
type result = S.context*S.term
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
  | (T 12) => "VAL"
  | (T 13) => "FN"
  | (T 14) => "ARROW"
  | (T 15) => "SEMI"
  | (T 16) => "ID"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 
1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.term term1, term1left, term1right)) :: 
rest671)) => let val  result = MlyValue.top (fn _ => let val  (term
 as term1) = term1 ()
 in (S.emptyCtx, term S.emptyCtx)
end)
 in ( LrTable.NT 0, ( result, term1left, term1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.term term1, _, term1right)) :: _ :: ( _, ( 
MlyValue.binds binds1, _, _)) :: ( _, ( _, VAL1left, _)) :: rest671))
 => let val  result = MlyValue.top (fn _ => let val  (binds as binds1)
 = binds1 ()
 val  (term as term1) = term1 ()
 in (binds, term binds)
end)
 in ( LrTable.NT 0, ( result, VAL1left, term1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.binds (fn _ => let val  (ID as ID1) = ID1
 ()
 in (S.addName S.emptyCtx ID)
end)
 in ( LrTable.NT 1, ( result, ID1left, ID1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.binds binds1, _, binds1right)) :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.binds (fn _ => let val  (ID as ID1) = ID1 ()
 val  (binds as binds1) = binds1 ()
 in (S.addName binds ID)
end)
 in ( LrTable.NT 1, ( result, ID1left, binds1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.appterm appterm1, appterm1left, 
appterm1right)) :: rest671)) => let val  result = MlyValue.term (fn _
 => let val  (appterm as appterm1) = appterm1 ()
 in (appterm)
end)
 in ( LrTable.NT 2, ( result, appterm1left, appterm1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.term term1, _, term1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, FN1left, _)) :: rest671)) => let
 val  result = MlyValue.term (fn _ => let val  (ID as ID1) = ID1 ()
 val  (term as term1) = term1 ()
 in (
fn ctx => let val ctx' = S.addName ctx ID
                                     in  S.TmAbs (ID, term ctx')
                                     end
)
end)
 in ( LrTable.NT 2, ( result, FN1left, term1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.atterm atterm1, atterm1left, atterm1right))
 :: rest671)) => let val  result = MlyValue.appterm (fn _ => let val 
 (atterm as atterm1) = atterm1 ()
 in (atterm)
end)
 in ( LrTable.NT 3, ( result, atterm1left, atterm1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.atterm atterm1, _, atterm1right)) :: ( _, ( 
MlyValue.appterm appterm1, appterm1left, _)) :: rest671)) => let val  
result = MlyValue.appterm (fn _ => let val  (appterm as appterm1) = 
appterm1 ()
 val  (atterm as atterm1) = atterm1 ()
 in (fn ctx => S.TmApp (appterm ctx, atterm ctx))
end)
 in ( LrTable.NT 3, ( result, appterm1left, atterm1right), rest671)

end
|  ( 8, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.term term1, _
, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.atterm (fn _ => let val  (term as term1) = term1 ()
 in (term)
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.atterm (fn _ => let val  (ID as ID1) = ID1
 ()
 in (fn ctx => S.TmVar (S.nameToIdx ctx ID, length ctx))
end)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.top x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Untyped_TOKENS =
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
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
end
end
