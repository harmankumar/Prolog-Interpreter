functor CalcLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calc_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\021\000\002\000\020\000\003\000\019\000\005\000\018\000\
\\006\000\017\000\013\000\016\000\018\000\015\000\019\000\043\000\
\\032\000\014\000\000\000\
\\001\000\001\000\021\000\002\000\020\000\003\000\019\000\005\000\018\000\
\\006\000\017\000\013\000\016\000\018\000\015\000\032\000\014\000\000\000\
\\001\000\007\000\000\000\009\000\000\000\000\000\
\\001\000\008\000\039\000\014\000\038\000\015\000\037\000\016\000\036\000\
\\020\000\035\000\021\000\034\000\022\000\033\000\023\000\032\000\
\\024\000\031\000\025\000\030\000\027\000\029\000\028\000\028\000\
\\029\000\027\000\030\000\026\000\031\000\025\000\033\000\024\000\000\000\
\\001\000\008\000\046\000\000\000\
\\001\000\008\000\068\000\000\000\
\\001\000\012\000\072\000\000\000\
\\001\000\014\000\038\000\015\000\037\000\019\000\073\000\020\000\035\000\
\\021\000\034\000\022\000\033\000\023\000\032\000\024\000\031\000\
\\025\000\030\000\027\000\029\000\028\000\028\000\029\000\027\000\
\\030\000\026\000\031\000\025\000\033\000\024\000\000\000\
\\001\000\019\000\063\000\000\000\
\\001\000\026\000\009\000\034\000\008\000\000\000\
\\075\000\000\000\
\\076\000\026\000\009\000\034\000\008\000\000\000\
\\077\000\000\000\
\\078\000\010\000\047\000\014\000\038\000\015\000\037\000\020\000\035\000\
\\021\000\034\000\022\000\033\000\023\000\032\000\024\000\031\000\
\\025\000\030\000\027\000\029\000\028\000\028\000\029\000\027\000\
\\030\000\026\000\031\000\025\000\033\000\024\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\010\000\065\000\014\000\038\000\015\000\037\000\017\000\064\000\
\\020\000\035\000\021\000\034\000\022\000\033\000\023\000\032\000\
\\024\000\031\000\025\000\030\000\027\000\029\000\028\000\028\000\
\\029\000\027\000\030\000\026\000\031\000\025\000\033\000\024\000\000\000\
\\089\000\010\000\065\000\014\000\038\000\015\000\037\000\020\000\035\000\
\\021\000\034\000\022\000\033\000\023\000\032\000\024\000\031\000\
\\025\000\030\000\027\000\029\000\028\000\028\000\029\000\027\000\
\\030\000\026\000\031\000\025\000\033\000\024\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\020\000\035\000\021\000\034\000\022\000\033\000\023\000\032\000\
\\024\000\031\000\025\000\030\000\030\000\026\000\033\000\024\000\000\000\
\\095\000\014\000\038\000\015\000\037\000\020\000\035\000\021\000\034\000\
\\022\000\033\000\023\000\032\000\024\000\031\000\025\000\030\000\
\\027\000\029\000\028\000\028\000\029\000\027\000\030\000\026\000\
\\031\000\025\000\033\000\024\000\000\000\
\\096\000\020\000\035\000\021\000\034\000\022\000\033\000\023\000\032\000\
\\024\000\031\000\025\000\030\000\027\000\029\000\028\000\028\000\
\\029\000\027\000\030\000\026\000\031\000\025\000\033\000\024\000\000\000\
\\097\000\020\000\035\000\021\000\034\000\022\000\033\000\023\000\032\000\
\\024\000\031\000\025\000\030\000\027\000\029\000\028\000\028\000\
\\029\000\027\000\030\000\026\000\031\000\025\000\033\000\024\000\000\000\
\\098\000\014\000\038\000\015\000\037\000\020\000\035\000\021\000\034\000\
\\022\000\033\000\023\000\032\000\024\000\031\000\025\000\030\000\
\\027\000\029\000\028\000\028\000\029\000\027\000\030\000\026\000\
\\031\000\025\000\033\000\024\000\000\000\
\\099\000\014\000\038\000\015\000\037\000\020\000\035\000\021\000\034\000\
\\022\000\033\000\023\000\032\000\024\000\031\000\025\000\030\000\
\\027\000\029\000\028\000\028\000\029\000\027\000\030\000\026\000\
\\031\000\025\000\033\000\024\000\000\000\
\\100\000\014\000\038\000\015\000\037\000\020\000\035\000\021\000\034\000\
\\022\000\033\000\023\000\032\000\024\000\031\000\025\000\030\000\
\\027\000\029\000\028\000\028\000\029\000\027\000\030\000\026\000\
\\031\000\025\000\033\000\024\000\000\000\
\\101\000\014\000\038\000\015\000\037\000\020\000\035\000\021\000\034\000\
\\022\000\033\000\023\000\032\000\024\000\031\000\025\000\030\000\
\\027\000\029\000\028\000\028\000\029\000\027\000\030\000\026\000\
\\031\000\025\000\033\000\024\000\000\000\
\\102\000\014\000\038\000\015\000\037\000\020\000\035\000\021\000\034\000\
\\022\000\033\000\023\000\032\000\024\000\031\000\025\000\030\000\
\\027\000\029\000\028\000\028\000\029\000\027\000\030\000\026\000\
\\031\000\025\000\033\000\024\000\000\000\
\\103\000\014\000\038\000\015\000\037\000\020\000\035\000\021\000\034\000\
\\022\000\033\000\023\000\032\000\024\000\031\000\025\000\030\000\
\\027\000\029\000\028\000\028\000\029\000\027\000\030\000\026\000\
\\031\000\025\000\033\000\024\000\000\000\
\\104\000\020\000\035\000\021\000\034\000\022\000\033\000\023\000\032\000\
\\024\000\031\000\025\000\030\000\029\000\027\000\030\000\026\000\
\\031\000\025\000\033\000\024\000\000\000\
\\105\000\020\000\035\000\021\000\034\000\022\000\033\000\023\000\032\000\
\\024\000\031\000\025\000\030\000\029\000\027\000\030\000\026\000\
\\031\000\025\000\033\000\024\000\000\000\
\\106\000\020\000\035\000\021\000\034\000\022\000\033\000\023\000\032\000\
\\024\000\031\000\025\000\030\000\030\000\026\000\033\000\024\000\000\000\
\\107\000\020\000\035\000\021\000\034\000\022\000\033\000\023\000\032\000\
\\024\000\031\000\025\000\030\000\030\000\026\000\033\000\024\000\000\000\
\\108\000\014\000\038\000\015\000\037\000\020\000\035\000\021\000\034\000\
\\022\000\033\000\023\000\032\000\024\000\031\000\025\000\030\000\
\\027\000\029\000\028\000\028\000\029\000\027\000\030\000\026\000\
\\031\000\025\000\033\000\024\000\000\000\
\\109\000\020\000\035\000\021\000\034\000\022\000\033\000\023\000\032\000\
\\024\000\031\000\025\000\030\000\030\000\026\000\033\000\024\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\011\000\045\000\000\000\
\"
val actionRowNumbers =
"\009\000\017\000\016\000\015\000\
\\011\000\010\000\001\000\001\000\
\\012\000\027\000\029\000\003\000\
\\001\000\000\000\001\000\028\000\
\\050\000\048\000\049\000\047\000\
\\004\000\013\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\018\000\031\000\008\000\
\\024\000\021\000\030\000\001\000\
\\020\000\001\000\045\000\042\000\
\\044\000\043\000\041\000\040\000\
\\037\000\039\000\038\000\036\000\
\\035\000\034\000\005\000\033\000\
\\032\000\023\000\001\000\001\000\
\\006\000\014\000\019\000\007\000\
\\026\000\025\000\046\000\022\000\
\\002\000"
val gotoT =
"\
\\001\000\072\000\002\000\005\000\003\000\004\000\004\000\003\000\
\\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\008\000\003\000\004\000\004\000\003\000\005\000\002\000\
\\006\000\001\000\000\000\
\\000\000\
\\007\000\011\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\021\000\008\000\020\000\010\000\010\000\011\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\038\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\040\000\009\000\039\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\042\000\010\000\010\000\011\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\046\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\047\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\048\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\049\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\050\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\051\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\052\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\053\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\054\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\055\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\056\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\057\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\021\000\008\000\058\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\059\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\060\000\010\000\010\000\011\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\021\000\008\000\064\000\010\000\010\000\011\000\009\000\000\000\
\\000\000\
\\007\000\021\000\008\000\065\000\010\000\010\000\011\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\067\000\010\000\010\000\011\000\009\000\000\000\
\\007\000\069\000\009\000\068\000\010\000\010\000\011\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 73
val numrules = 40
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
datatype svalue = VOID | ntVOID of unit | UCID of  (string)
 | LCID of  (string) | RATIONAL of  (real) | NUM of  (int)
 | BOOLEAN of  (bool) | Constant of  (Absyn.const)
 | tylist of  (Absyn.optree) | tylist1 of  (Absyn.optree)
 | Optreelist of  ( (  Absyn.optree list  ) )
 | Optree of  (Absyn.optree) | Goal of  (Absyn.declaration)
 | Rule of  (Absyn.declaration) | Fact of  (Absyn.declaration)
 | Declaration of  (Absyn.declaration)
 | Declarationlist of  ( ( Absyn.declaration list ) )
 | Program of  (Absyn.program)
end
type svalue = MlyValue.svalue
type result = Absyn.program
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 8) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 7))::
(nil
,nil
 $$ (T 13))::
(nil
,nil
 $$ (T 14))::
(nil
,nil
 $$ (T 28))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 26))::
(nil
,nil
 $$ (T 27))::
nil
val noShift = 
fn (T 6) => true | _ => false
val showTerminal =
fn (T 0) => "BOOLEAN"
  | (T 1) => "NUM"
  | (T 2) => "RATIONAL"
  | (T 3) => "FUNC"
  | (T 4) => "LCID"
  | (T 5) => "UCID"
  | (T 6) => "EOF"
  | (T 7) => "DOT"
  | (T 8) => "SEMI"
  | (T 9) => "COMMA"
  | (T 10) => "LPAREN"
  | (T 11) => "RPAREN"
  | (T 12) => "NOT"
  | (T 13) => "AND"
  | (T 14) => "OR"
  | (T 15) => "IF"
  | (T 16) => "CONS"
  | (T 17) => "LSOLIDPAREN"
  | (T 18) => "RSOLIDPAREN"
  | (T 19) => "GT"
  | (T 20) => "LT"
  | (T 21) => "EQ"
  | (T 22) => "GTEQ"
  | (T 23) => "LTEQ"
  | (T 24) => "NEQ"
  | (T 25) => "QUERY"
  | (T 26) => "PLUS"
  | (T 27) => "SUB"
  | (T 28) => "DIV"
  | (T 29) => "MOD"
  | (T 30) => "MULT"
  | (T 31) => "ABS"
  | (T 32) => "EXP"
  | (T 33) => "IMPOS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Declarationlist Declarationlist, 
Declarationlist1left, Declarationlist1right)) :: rest671)) => let val 
 result = MlyValue.Program ( Absyn.PROGRAM( Declarationlist ))
 in ( LrTable.NT 0, ( result, Declarationlist1left, 
Declarationlist1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Declaration Declaration, Declaration1left, 
Declaration1right)) :: rest671)) => let val  result = 
MlyValue.Declarationlist ( [Declaration] )
 in ( LrTable.NT 1, ( result, Declaration1left, Declaration1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.Declarationlist Declarationlist, _, 
Declarationlist1right)) :: ( _, ( MlyValue.Declaration Declaration, 
Declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.Declarationlist ( Declaration::Declarationlist )
 in ( LrTable.NT 1, ( result, Declaration1left, Declarationlist1right)
, rest671)
end
|  ( 3, ( ( _, ( MlyValue.Optree Optree, Optree1left, Optree1right))
 :: rest671)) => let val  result = MlyValue.Optreelist ( [Optree] )
 in ( LrTable.NT 7, ( result, Optree1left, Optree1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Optreelist Optreelist, _, Optreelist1right))
 :: _ :: ( _, ( MlyValue.Optree Optree, Optree1left, _)) :: rest671))
 => let val  result = MlyValue.Optreelist ( Optree::Optreelist )
 in ( LrTable.NT 7, ( result, Optree1left, Optreelist1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.Fact Fact, Fact1left, Fact1right)) :: 
rest671)) => let val  result = MlyValue.Declaration ( Fact )
 in ( LrTable.NT 2, ( result, Fact1left, Fact1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.Rule Rule, Rule1left, Rule1right)) :: 
rest671)) => let val  result = MlyValue.Declaration ( Rule )
 in ( LrTable.NT 2, ( result, Rule1left, Rule1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.Goal Goal, Goal1left, Goal1right)) :: 
rest671)) => let val  result = MlyValue.Declaration ( Goal )
 in ( LrTable.NT 2, ( result, Goal1left, Goal1right), rest671)
end
|  ( 8, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.Optree Optree, _
, _)) :: ( _, ( _, IMPOS1left, _)) :: rest671)) => let val  result = 
MlyValue.Fact ( Absyn.FACT( Optree ))
 in ( LrTable.NT 3, ( result, IMPOS1left, DOT1right), rest671)
end
|  ( 9, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.Optreelist 
Optreelist, _, _)) :: _ :: ( _, ( MlyValue.Optree Optree, _, _)) :: (
 _, ( _, IMPOS1left, _)) :: rest671)) => let val  result = 
MlyValue.Rule (Absyn.RULE( Optree, Optreelist))
 in ( LrTable.NT 4, ( result, IMPOS1left, DOT1right), rest671)
end
|  ( 10, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.Optreelist 
Optreelist, _, _)) :: ( _, ( _, QUERY1left, _)) :: rest671)) => let
 val  result = MlyValue.Goal (Absyn.GOAL(Optreelist))
 in ( LrTable.NT 5, ( result, QUERY1left, DOT1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RSOLIDPAREN1right)) :: ( _, ( _, 
LSOLIDPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.tylist ( Absyn.NONVARIABLE( Absyn.LCID ("empty")) )
 in ( LrTable.NT 9, ( result, LSOLIDPAREN1left, RSOLIDPAREN1right), 
rest671)
end
|  ( 12, ( ( _, ( _, _, RSOLIDPAREN1right)) :: ( _, ( MlyValue.Optree 
Optree2, _, _)) :: _ :: ( _, ( MlyValue.Optree Optree1, _, _)) :: ( _,
 ( _, LSOLIDPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.tylist ( Absyn.NODE("Listof " ,  Optree1::[Optree2] ))
 in ( LrTable.NT 9, ( result, LSOLIDPAREN1left, RSOLIDPAREN1right), 
rest671)
end
|  ( 13, ( ( _, ( _, _, RSOLIDPAREN1right)) :: ( _, ( MlyValue.tylist1
 tylist1, _, _)) :: ( _, ( _, LSOLIDPAREN1left, _)) :: rest671)) =>
 let val  result = MlyValue.tylist ( tylist1 )
 in ( LrTable.NT 9, ( result, LSOLIDPAREN1left, RSOLIDPAREN1right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.Optree Optree, Optree1left, Optree1right))
 :: rest671)) => let val  result = MlyValue.tylist1 (
 Absyn.NODE("Listof " , Optree::[Absyn.NONVARIABLE( Absyn.LCID("empty"))]  ) 
)
 in ( LrTable.NT 8, ( result, Optree1left, Optree1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.tylist1 tylist1, _, tylist11right)) :: _ ::
 ( _, ( MlyValue.Optree Optree, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.tylist1 (
 Absyn.NODE("Listof " , Optree::[tylist1] ))
 in ( LrTable.NT 8, ( result, Optree1left, tylist11right), rest671)

end
|  ( 16, ( ( _, ( MlyValue.Constant Constant, Constant1left, 
Constant1right)) :: rest671)) => let val  result = MlyValue.Optree (
Absyn.NONVARIABLE(Constant))
 in ( LrTable.NT 6, ( result, Constant1left, Constant1right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.UCID UCID, UCID1left, UCID1right)) :: 
rest671)) => let val  result = MlyValue.Optree (Absyn.VARIABLE(UCID))
 in ( LrTable.NT 6, ( result, UCID1left, UCID1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.tylist tylist, tylist1left, tylist1right))
 :: rest671)) => let val  result = MlyValue.Optree ( tylist )
 in ( LrTable.NT 6, ( result, tylist1left, tylist1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Optree Optree, _, Optree1right)) :: ( _, (
 _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.Optree (
 Absyn.NODE("not" , [Optree]))
 in ( LrTable.NT 6, ( result, NOT1left, Optree1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.Optree Optree, _, Optree1right)) :: ( _, (
 _, ABS1left, _)) :: rest671)) => let val  result = MlyValue.Optree (
 Absyn.NODE("abs" , [Optree]))
 in ( LrTable.NT 6, ( result, ABS1left, Optree1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "and" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "or" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "gt" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "lt" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "eq" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "neq" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "gteq" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "lteq" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "add" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "sub" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "mult" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "div" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "mod" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.Optree Optree2, _, Optree2right)) :: _ :: (
 _, ( MlyValue.Optree Optree1, Optree1left, _)) :: rest671)) => let
 val  result = MlyValue.Optree (
 Absyn.NODE( "exp" , [Optree1 , Optree2]))
 in ( LrTable.NT 6, ( result, Optree1left, Optree2right), rest671)
end
|  ( 35, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Optreelist 
Optreelist, _, _)) :: _ :: ( _, ( MlyValue.LCID LCID, LCID1left, _))
 :: rest671)) => let val  result = MlyValue.Optree (
 Absyn.NODE( LCID,Optreelist) )
 in ( LrTable.NT 6, ( result, LCID1left, RPAREN1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.BOOLEAN BOOLEAN, BOOLEAN1left, 
BOOLEAN1right)) :: rest671)) => let val  result = MlyValue.Constant (
Absyn.BOOL(BOOLEAN))
 in ( LrTable.NT 10, ( result, BOOLEAN1left, BOOLEAN1right), rest671)

end
|  ( 37, ( ( _, ( MlyValue.RATIONAL RATIONAL, RATIONAL1left, 
RATIONAL1right)) :: rest671)) => let val  result = MlyValue.Constant (
Absyn.RATIONAL(RATIONAL))
 in ( LrTable.NT 10, ( result, RATIONAL1left, RATIONAL1right), rest671
)
end
|  ( 38, ( ( _, ( MlyValue.NUM NUM, NUM1left, NUM1right)) :: rest671))
 => let val  result = MlyValue.Constant (Absyn.INT(NUM))
 in ( LrTable.NT 10, ( result, NUM1left, NUM1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.LCID LCID, LCID1left, LCID1right)) :: 
rest671)) => let val  result = MlyValue.Constant (Absyn.LCID(LCID))
 in ( LrTable.NT 10, ( result, LCID1left, LCID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun BOOLEAN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.BOOLEAN i,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM i,p1,p2))
fun RATIONAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.RATIONAL i,p1,p2))
fun FUNC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun LCID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.LCID i,p1,p2))
fun UCID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.UCID i,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun CONS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LSOLIDPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun RSOLIDPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GTEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LTEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun QUERY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun MULT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun ABS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun EXP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPOS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
end
end
