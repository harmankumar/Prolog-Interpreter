structure unif = struct


datatype symbol = VARIABLE of string | NONVARIABLE of string
type arity = int
type sign = (symbol*arity) list
fun uncurry_normal (f) (a) (b) = f(a,b);

val mysig = [(NONVARIABLE("0"),0),(NONVARIABLE("1"),0),(NONVARIABLE("+"),2),(VARIABLE("x"),0)]

fun test(x,y) = if x < y then y+1 else x+1

fun getarity (_,nil) = ~1
	|getarity(x,(NONVARIABLE(a),b)::xs) = if(x=a) then b else getarity(x,xs)
	|getarity(x,(VARIABLE(a),b)::xs) = if(x=a) then b else getarity(x,xs)


fun checkdiff(_,nil) = true
	|checkdiff ((NONVARIABLE(x)),(NONVARIABLE(a),b)::xs) = if (x<>a) then checkdiff((NONVARIABLE(x)),xs) else false
	|checkdiff ((NONVARIABLE(x)),(VARIABLE(a),b)::xs) = if (x<>a andalso b=0 ) then checkdiff((NONVARIABLE(x)),xs) else false
	|checkdiff ((VARIABLE(x)),(NONVARIABLE(a),b)::xs) = if (x<>a) then checkdiff((NONVARIABLE(x)),xs) else false
	|checkdiff ((VARIABLE(x)),(VARIABLE(a),b)::xs) = if (x<>a andalso b=0) then checkdiff((NONVARIABLE(x)),xs) else false;

fun check_sig(nil) = true
	|check_sig((sy,ar)::xs) = if((ar>=0) andalso checkdiff(sy,xs)) then check_sig(xs) else false


(* use map for getting the result for all the members of the list *)
(* Part 2 *)

datatype treesigma = LEAF of symbol | NODE of string*(treesigma list)
(*  treesigma = Absyn.NONVARIABLE of string | Absyn.VARIABLE of string | Absyn.FUNCTION of string*(optree list) *)



fun wff(sigma:sign)(LEAF(VARIABLE(x)):treesigma) = if((getarity (x,sigma))=0) then true else false
	|wff(sigma:sign)(LEAF(NONVARIABLE(x)):treesigma) = if((getarity (x,sigma))=0) then true else false	
	|wff(sigma:sign)(NODE(x,y):treesigma) = if((getarity (x,sigma)) = List.length(y))then (List.foldl (fn(a,b)=>a andalso b) true (List.map (wff(sigma)) y)) else false;

(*|wff(sigm,(a,lis)) = if( (getarity(a,sigm)= length(lis)) andalso ( List.all (revcur (wff) (sigm)) arglist ) ) then true else false*) 


(* Part 3 and 4 *)

open Absyn

type substitution = optree*optree

type mapping = substitution list

fun substontree(func : optree -> optree) (NODE(x,y)) = NODE(x , (List.map (substontree func)  y))
	|substontree (func : optree -> optree) (NONVARIABLE(x)) = NONVARIABLE(x)
	|substontree (func : optree -> optree) (VARIABLE(x)) = func(VARIABLE(x))
	
local
	exception WRONGSUBST;
	exception UNDEFINEDVARIABLE;
	
	fun lookfor (VARIABLE(x)) (nil) = VARIABLE(x)
		|lookfor (VARIABLE(x)) ((VARIABLE(a),b:optree)::xs) = if(a=x) then b else lookfor (VARIABLE(x)) (xs);
	
in
	fun substfunc (lookup:mapping)(NONVARIABLE(x)) = NONVARIABLE(x)
		|substfunc(lookup:mapping)(VARIABLE(x)) =  lookfor (VARIABLE(x)) (lookup)
		|substfunc(lookup:mapping) (_) = raise WRONGSUBST; 
end

(* val answer=substontree (substfunc (mymapping)) (OP3) *)


(* Part 5 *)
exception FAILUNIFY
exception UNIQUENESSHINDERED



(* make it more efficient *)


	fun belongs (x) (xs) = Option.isSome( List.find (uncurry_normal (op=) (x) ) (xs));

	fun contains([],x) = false
	|contains(VARIABLE(x)::xs , VARIABLE(y)) = if(x=y) then true else contains(xs,VARIABLE(y))	

	fun make_set([]) (lis) = lis
		|make_set(x::xs) (lis) = if(contains(lis,x)) then make_set(xs) (lis) else (make_set (xs) (x::lis))	


	fun myhelper (NONVARIABLE(_)) (ans) = ans
		|myhelper (VARIABLE(x)) (ans) = ((VARIABLE(x))::(ans)) 
		|myhelper (NODE(x , treelist : optree list)) (ans) = 
								(List.foldl (op@) (ans) (List.map (vars) (treelist))) 
	and vars mytree = make_set (myhelper (mytree) ([] : optree list)) ([]);




(* if (belongs (VARIABLE(x)) (vars(NODE(a,b)) ) then  varispres else varnotpres  *)

fun expect (INT(x)) = INT(x+1)
	|expect(BOOL(x)) = BOOL(x)

	fun checkpres(y,x::xs) = if(x=y) then true else checkpres(y,xs)
		|checkpres(y,[]) = false


fun getuniqlist(x::xs,lis) = if(checkpres(x,lis)) then getuniqlist(xs,lis) else getuniqlist(xs,x::lis)
	|getuniqlist([],lis) = lis 

fun allvars(x::xs , lis) = allvars(xs , getuniqlist(lis , lis))
	|allvars([] , lis) = lis

fun checknotpresent(VARIABLE(y), VARIABLE(x)::xs) = if(y = x) then false else checknotpresent(VARIABLE(y),xs)
	|checknotpresent(VARIABLE(y),[]) = true

fun removesum(SOME(x)) = x

fun helpcompose(_ , NONE) = NONE
	|helpcompose(NONE , _) = NONE
	|helpcompose(SOME([]) , SOME(x)) = SOME(x)
	|helpcompose(SOME(x),SOME([])) = SOME(x)
	|helpcompose(SOME((a,b)::xs) , SOME(y)) = SOME((a, substontree (substfunc (y)) (b)) :: removesum(helpcompose(SOME(xs),SOME(y))))


fun removecover(INT(x)) = INT(x)
	|removecover(RATIONAL(x)) = RATIONAL(x)
	|removecover(BOOL(x)) = BOOL(x)
	|removecover(LCID(x)) =LCID(x)

fun mostgenunif (  NODE(a,b),NODE(c,d)) = if( (a=c) andalso length(b)=length(d) ) then mostgenunifonlist(b,d,SOME(nil)) else NONE

	
	|mostgenunif ( NONVARIABLE(INT(x))  ,  NONVARIABLE(INT(y)) ) = if (x=y) then SOME([]) else NONE
	
	|mostgenunif ( NONVARIABLE(INT(x))  ,  NONVARIABLE(_)) = NONE
	
	
	|mostgenunif ( NONVARIABLE(BOOL(x))  ,  NONVARIABLE(BOOL(y)) ) = if (x=y) then SOME([]) else NONE
	
	|mostgenunif ( NONVARIABLE(BOOL(x))  ,  NONVARIABLE(_)) = NONE
	
	
	|mostgenunif ( NONVARIABLE(LCID(x))  ,  NONVARIABLE(LCID(y)) ) = if (x=y) then SOME([]) else NONE
	
	|mostgenunif ( NONVARIABLE(LCID(x))  ,  NONVARIABLE(_)) = NONE
	
	|mostgenunif ( NONVARIABLE(RATIONAL(x))  ,  NONVARIABLE(_)) = NONE
	

	|mostgenunif ( NONVARIABLE(x)  ,  VARIABLE(y) ) =  SOME( [(VARIABLE(y),NONVARIABLE(x))] )

	|mostgenunif (  VARIABLE(x),  NONVARIABLE(y)  ) =  SOME( [(VARIABLE(x),NONVARIABLE(y))] )

	|mostgenunif (  VARIABLE(x) ,  VARIABLE(y)  ) = if (x=y) then SOME([]) else SOME([(VARIABLE(x),VARIABLE(y))])

	|mostgenunif (  NONVARIABLE(x) ,  NODE( a,b )) = NONE

	|mostgenunif (  NODE( a,b ) , NONVARIABLE(x) ) = NONE

	|mostgenunif (  VARIABLE(x) ,  NODE( a,b ) ) = if (x=a) then NONE else ( if(contains ( vars(NODE(a,b)) , (VARIABLE(x)) ))  then NONE else SOME([(VARIABLE(x),NODE(a,b) )]))

	|mostgenunif (  NODE(a,b ) , VARIABLE(x)  ) = if (x=a) then NONE else ( if(contains ( vars(NODE(a,b)) , (VARIABLE(x)) ))  then NONE else SOME([(VARIABLE(x),NODE(a,b) )]))
(*)
	|mostgenunif (  NODE(a,b ) , VARIABLE(x)  ) = if (x=a) then NONE else ( if(uniq (VARIABLE(x)) (NODE(a,b)) )  then SOME([(VARIABLE(x),NODE(a,b) )]) else NONE)
*)
and mostgenunifonlist(_,_,NONE) = NONE
	|mostgenunifonlist([],[],SOME(acc)) = SOME(acc) (* Recur over the two lists, apply mgu on first two, apply on the rest ; do it for the xs list *)
	|mostgenunifonlist(x::xs,y::ys,SOME(acc)) =  mostgenunifonlist(xs , ys , helpcompose (SOME(acc), mostgenunif(substontree (substfunc (acc)) (x) , substontree (substfunc (acc)) (y)) ) )



(* Part 6 *)
(* Create two imaginary nodes with the k trees as the  children and apply mgu on the two *)

fun transform(mylist) = NODE("ranfun" , mylist)

fun applymguonlist ( mylist ) = mostgenunif( transform(List.drop(mylist , 1 )) , transform( List.take(mylist , length(mylist) - 1 )) )

end