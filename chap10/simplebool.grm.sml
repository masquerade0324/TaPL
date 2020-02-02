functor SimpleBoolLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : SimpleBool_TOKENS
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
\\001\000\001\000\012\000\004\000\011\000\005\000\010\000\010\000\009\000\
\\013\000\008\000\014\000\007\000\018\000\006\000\000\000\
\\001\000\001\000\012\000\004\000\011\000\005\000\010\000\010\000\009\000\
\\014\000\007\000\018\000\006\000\000\000\
\\001\000\002\000\051\000\003\000\051\000\004\000\011\000\005\000\010\000\
\\010\000\009\000\011\000\051\000\012\000\051\000\018\000\006\000\000\000\
\\001\000\002\000\052\000\003\000\052\000\011\000\052\000\012\000\052\000\000\000\
\\001\000\002\000\053\000\003\000\053\000\011\000\053\000\012\000\053\000\000\000\
\\001\000\002\000\054\000\003\000\054\000\004\000\054\000\005\000\054\000\
\\010\000\054\000\011\000\054\000\012\000\054\000\018\000\054\000\000\000\
\\001\000\002\000\055\000\003\000\055\000\004\000\055\000\005\000\055\000\
\\010\000\055\000\011\000\055\000\012\000\055\000\018\000\055\000\000\000\
\\001\000\002\000\056\000\003\000\056\000\004\000\056\000\005\000\056\000\
\\010\000\056\000\011\000\056\000\012\000\056\000\018\000\056\000\000\000\
\\001\000\002\000\057\000\003\000\057\000\004\000\057\000\005\000\057\000\
\\010\000\057\000\011\000\057\000\012\000\057\000\018\000\057\000\000\000\
\\001\000\002\000\058\000\003\000\058\000\004\000\058\000\005\000\058\000\
\\010\000\058\000\011\000\058\000\012\000\058\000\018\000\058\000\000\000\
\\001\000\002\000\059\000\003\000\059\000\004\000\059\000\005\000\059\000\
\\010\000\059\000\011\000\059\000\012\000\059\000\018\000\059\000\000\000\
\\001\000\002\000\023\000\000\000\
\\001\000\003\000\036\000\000\000\
\\001\000\010\000\028\000\017\000\027\000\000\000\
\\001\000\011\000\046\000\016\000\046\000\018\000\046\000\020\000\046\000\000\000\
\\001\000\011\000\047\000\015\000\032\000\016\000\047\000\018\000\047\000\
\\020\000\047\000\000\000\
\\001\000\011\000\048\000\016\000\048\000\018\000\048\000\020\000\048\000\000\000\
\\001\000\011\000\049\000\015\000\049\000\016\000\049\000\018\000\049\000\
\\020\000\049\000\000\000\
\\001\000\011\000\050\000\015\000\050\000\016\000\050\000\018\000\050\000\
\\020\000\050\000\000\000\
\\001\000\011\000\022\000\000\000\
\\001\000\011\000\039\000\000\000\
\\001\000\012\000\000\000\000\000\
\\001\000\012\000\042\000\000\000\
\\001\000\012\000\043\000\000\000\
\\001\000\016\000\044\000\018\000\016\000\000\000\
\\001\000\016\000\045\000\000\000\
\\001\000\016\000\020\000\000\000\
\\001\000\018\000\014\000\000\000\
\\001\000\018\000\016\000\000\000\
\\001\000\019\000\019\000\000\000\
\\001\000\019\000\021\000\000\000\
\\001\000\020\000\033\000\000\000\
\"
val actionRowNumbers =
"\000\000\005\000\002\000\022\000\
\\008\000\027\000\028\000\001\000\
\\010\000\009\000\001\000\006\000\
\\029\000\026\000\030\000\019\000\
\\011\000\013\000\001\000\013\000\
\\007\000\001\000\015\000\014\000\
\\031\000\018\000\013\000\023\000\
\\024\000\012\000\013\000\001\000\
\\020\000\025\000\001\000\016\000\
\\003\000\017\000\004\000\021\000"
val gotoT =
"\
\\001\000\039\000\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\008\000\011\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\013\000\000\000\
\\006\000\015\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\006\000\016\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\024\000\004\000\023\000\005\000\022\000\000\000\
\\006\000\027\000\007\000\002\000\008\000\001\000\000\000\
\\003\000\028\000\004\000\023\000\005\000\022\000\000\000\
\\000\000\
\\006\000\029\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\032\000\004\000\023\000\005\000\022\000\000\000\
\\000\000\
\\002\000\033\000\000\000\
\\000\000\
\\004\000\035\000\005\000\022\000\000\000\
\\006\000\036\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\006\000\038\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 40
val numrules = 18
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
 | term of unit ->  (S.context -> S.term) | atty of unit ->  (S.ty)
 | funty of unit ->  (S.ty) | ty of unit ->  (S.ty)
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
  | (T 16) => "BOOL"
  | (T 17) => "ID"
  | (T 18) => "COLON"
  | (T 19) => "DARROW"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 19) $$ (T 18) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
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
|  ( 2, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.binds (fn _ => let val  (ID as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in (S.addVar S.emptyCtx ID ty)
end)
 in ( LrTable.NT 1, ( result, ID1left, ty1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.binds binds1, _, binds1right)) :: ( _, ( 
MlyValue.ty ty1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _))
 :: rest671)) => let val  result = MlyValue.binds (fn _ => let val  (
ID as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 val  (binds as binds1) = binds1 ()
 in (S.addVar binds ID ty)
end)
 in ( LrTable.NT 1, ( result, ID1left, binds1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.funty funty1, funty1left, funty1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (funty
 as funty1) = funty1 ()
 in (funty)
end)
 in ( LrTable.NT 2, ( result, funty1left, funty1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.atty atty1, atty1left, atty1right)) :: 
rest671)) => let val  result = MlyValue.funty (fn _ => let val  (atty
 as atty1) = atty1 ()
 in (atty)
end)
 in ( LrTable.NT 3, ( result, atty1left, atty1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.funty funty1, _, funty1right)) :: _ :: ( _, 
( MlyValue.atty atty1, atty1left, _)) :: rest671)) => let val  result
 = MlyValue.funty (fn _ => let val  (atty as atty1) = atty1 ()
 val  (funty as funty1) = funty1 ()
 in (S.TyFun (atty, funty))
end)
 in ( LrTable.NT 3, ( result, atty1left, funty1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ty ty1, _, _)
) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.atty (fn _ => let val  (ty as ty1) = ty1 ()
 in (ty)
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 8, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.atty (fn _ => (S.TyBool))
 in ( LrTable.NT 4, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.appterm appterm1, appterm1left, 
appterm1right)) :: rest671)) => let val  result = MlyValue.term (fn _
 => let val  (appterm as appterm1) = appterm1 ()
 in (appterm)
end)
 in ( LrTable.NT 5, ( result, appterm1left, appterm1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.term term1, _, term1right)) :: _ :: ( _, ( 
MlyValue.ty ty1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _,
 ( _, FN1left, _)) :: rest671)) => let val  result = MlyValue.term (fn
 _ => let val  (ID as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 val  (term as term1) = term1 ()
 in (
fn ctx => let val ctx' = S.addName ctx ID
                                     in  S.TmAbs (ID, ty, term ctx')
                                     end
)
end)
 in ( LrTable.NT 5, ( result, FN1left, term1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.term term3, _, term3right)) :: _ :: ( _, ( 
MlyValue.term term2, _, _)) :: _ :: ( _, ( MlyValue.term term1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.term (fn _ => let val  term1 = term1 ()
 val  term2 = term2 ()
 val  term3 = term3 ()
 in (
fn ctx => S.TmIf (term1 ctx,
                                             term2 ctx,
                                             term3 ctx)
)
end)
 in ( LrTable.NT 5, ( result, IF1left, term3right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.atterm atterm1, atterm1left, atterm1right))
 :: rest671)) => let val  result = MlyValue.appterm (fn _ => let val 
 (atterm as atterm1) = atterm1 ()
 in (atterm)
end)
 in ( LrTable.NT 6, ( result, atterm1left, atterm1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.atterm atterm1, _, atterm1right)) :: ( _, (
 MlyValue.appterm appterm1, appterm1left, _)) :: rest671)) => let val 
 result = MlyValue.appterm (fn _ => let val  (appterm as appterm1) = 
appterm1 ()
 val  (atterm as atterm1) = atterm1 ()
 in (fn ctx => S.TmApp (appterm ctx, atterm ctx))
end)
 in ( LrTable.NT 6, ( result, appterm1left, atterm1right), rest671)

end
|  ( 14, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.term term1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.atterm (fn _ => let val  (term as term1) = term1 ()
 in (term)
end)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.atterm (fn _ => let val  (ID as ID1) = ID1
 ()
 in (fn ctx => S.TmVar (S.nameToIdx ctx ID, length ctx))
end)
 in ( LrTable.NT 7, ( result, ID1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.atterm (fn _ => (fn ctx => S.TmTrue))
 in ( LrTable.NT 7, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 17, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.atterm (fn _ => (fn ctx => S.TmFalse))
 in ( LrTable.NT 7, ( result, FALSE1left, FALSE1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.top x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : SimpleBool_TOKENS =
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
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
end
end
