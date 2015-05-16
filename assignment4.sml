structure assignment4 = struct 

(* Segregate out the queries, rules and facts. *)

open Absyn

fun curriedhelpcompose (x) (y) = unif.helpcompose(x,y)

fun revcur (f) (a) (b) = f (b) (a)

fun lifted_append ([]) = ([])
		|lifted_append (l::ls) = (l@(lifted_append ls));

fun appsome(x) = SOME(x)

fun remgoal ([] , lis) = lis
	|remgoal( FACT(x)::xs , lis ) = remgoal ( xs , FACT(x)::lis)
	|remgoal( RULE(a,b)::xs ,lis) = remgoal ( xs , RULE(a,b)::lis)
	|remgoal( GOAL(x)::xs , lis ) = remgoal ( xs , lis)

fun corrquerord ([] , lis) = lis
	|corrquerord( FACT(x)::xs , lis ) = corrquerord ( xs , lis)
	|corrquerord( RULE(a,b)::xs ,lis) = corrquerord ( xs , lis)
	|corrquerord( GOAL(x)::xs , lis ) = corrquerord ( xs , lis@[GOAL(x)])


fun getgoals ([] , lis) = lis
	|getgoals( FACT(x)::xs , lis ) = getgoals ( xs , lis)
	|getgoals( RULE(a,b)::xs ,lis) = getgoals ( xs , lis)
	|getgoals( GOAL(x)::xs , lis ) = getgoals ( xs , GOAL(x)::lis)

fun unifyoneagainstprogram  ( x : Absyn.optree , [] , compdecl) = []
	|unifyoneagainstprogram ( x : Absyn.optree , FACT(f)::xs , compdecl ) = let val tempval = unif.mostgenunif(f , x)
																		in	
																		  if(Option.isSome(tempval)) 
																		  then Option.valOf(tempval) :: unifyoneagainstprogram( x , xs , compdecl )
																	      else unifyoneagainstprogram( x , xs , compdecl )
																	    end 	

	|unifyoneagainstprogram( x : Absyn.optree , RULE(hd , tl)::xs , compdecl) = 


		let val firsttemp = unif.mostgenunif( x,hd )
		in
			if(Option.isSome(firsttemp))
			then 
				(* Apply whatever is recieved from above to the rule list
				   Call the list function so that rule could be checked. *)	
				let 
					val newrule = List.map (unif.substontree (unif.substfunc (Option.valOf(firsttemp)))) (tl) 
					val apprule = unifylistagainstprogram( newrule , compdecl)
				in 
					(* fill this *)
					if(List.null(apprule)) 
					then 
						unifyoneagainstprogram( x , xs , compdecl )
					else  
						let val tempcur =  curriedhelpcompose (firsttemp)
						in 
							(List.map (unif.removesum) (List.map ( tempcur ) ( List.map (appsome) (apprule) ) ) ) @ unifyoneagainstprogram( x , xs , compdecl )
							(* unif.removesum ( tempcur List.map (appsome)s (SOME(apprule) )):: unifyoneagainstprogram( x , xs , compdecl )*)
						end
				end

			else unifyoneagainstprogram( x , xs , compdecl )
		end 

	|unifyoneagainstprogram( x : Absyn.optree , GOAL(lis)::xs , compdecl) = unifyoneagainstprogram(x,xs,compdecl)

(* This function calls the other one with   optree , prog *)
and  unifylistagainstprogram( [], prog ) = []                   (* NOT SURE  *) 
	|unifylistagainstprogram( [elem], prog ) = unifyoneagainstprogram ( elem , prog , prog)
	|unifylistagainstprogram( elem::elemlist , prog) = 
	let
		 val tempunif = unifyoneagainstprogram( elem , prog , prog )
	in 
		if(List.null(tempunif))
		then ([])
		else 
				let
					fun helperfunc (remlist) (prog) (qu) = (* qs is the list of queries yet to unify, qu is a single unifier that we will test.*)
						let
							val modified_remlist = List.map (unif.substontree (unif.substfunc (qu))) (remlist)
							val modified_qs_unifiers = unifylistagainstprogram (modified_remlist , prog)
						in
							if(List.null(modified_qs_unifiers)) then []
							else List.map (unif.removesum) (List.map (  (curriedhelpcompose) (SOME(qu))) ( List.map (appsome) (modified_qs_unifiers)))
						end												(* CHECK *)
				in
					lifted_append (List.map ( helperfunc (elemlist) (prog)) (tempunif) )
				end
		end



	and InterpretSingle(query: Absyn.optree)(prog: Absyn.declaration list)([]: Absyn.declaration list)  = []
		|InterpretSingle(query: Absyn.optree)(prog: Absyn.declaration list)(Absyn.FACT(x)::xs:  Absyn.declaration list) = let
																					val tmp = unif.mostgenunif(query,x)
																				  in
																				  	if (Option.isSome(tmp)) 
																				  	then (Option.valOf(tmp))::InterpretSingle(query)(prog)(xs) 
																				  	else InterpretSingle(query)(prog)(xs)
																				  end

		
		|InterpretSingle(query: Absyn.optree)(prog: Absyn.declaration list)(Absyn.RULE((a,b))::xs:  Absyn.declaration list) = let 
																					val substHead = unif.mostgenunif(a,query)
																				  in
																				  	if(Option.isSome(substHead)) 
																				  	then Interpretlis(prog)(List.map (unif.substontree (unif.substfunc (Option.valOf(substHead) ) ) ) b) @ InterpretSingle(query)(prog)(xs)
																				  	else InterpretSingle(query)(prog)(xs)
																				  end
		and Interpretlis(prog: Absyn.declaration list) (nil) = []
			|Interpretlis(prog: Absyn.declaration list)(x::nil) = InterpretSingle(x)(prog)(prog)
			|Interpretlis(prog: Absyn.declaration list)(x::xs: Absyn.optree list) = InterpretlisHelper(prog)(xs)(InterpretSingle(x)(prog)(prog))
			
		and InterpretlisHelper(prog)(xs)(nil) = []
			|InterpretlisHelper(prog: Absyn.declaration list)(xs: Absyn.optree list)(y::ys) = (List.map (fn(x)=> unif.removesum (unif.helpcompose(appsome (y),appsome (x)) ) )  (Interpretlis(prog)(List.map (unif.substontree (unif.substfunc (y) ) )  xs) ) )@ (InterpretlisHelper(prog)(xs)(ys))

		
		fun InterpreterDriverHelper(prog)(nil) = nil
			|InterpreterDriverHelper(prog: Absyn.declaration list)(Absyn.GOAL(y)::xs) = ((Interpretlis(prog)(y))) :: (InterpreterDriverHelper(prog: Absyn.declaration list)(xs))
	
		fun InterpreterDriver(Absyn.PROGRAM(x): Absyn.program) = InterpreterDriverHelper(remgoal (x , [])) (corrquerord (x , []));




fun unwrap (PROGRAM(x)) = x;




fun execute( [] , allprov ) = []
	|execute(GOAL(gl)::remgl , allprov) = unifylistagainstprogram ( gl , allprov ) :: execute(remgl , allprov)

fun driver( x ) = execute(getgoals(unwrap(x) , []) , remgoal(unwrap(x) , []) )  (*  List.map () *)




     (*  FOR GIVING PROLOG LIKE OUTPUT *)


	fun optree2string (VARIABLE(vn)) = vn^" "
		|optree2string (NONVARIABLE(INT(n))) = Int.toString(n)^" "
		|optree2string (NONVARIABLE(RATIONAL(n))) = Real.toString(n)^" "
		|optree2string (NONVARIABLE(LCID(n))) = (n)^" "
		|optree2string (NONVARIABLE(BOOL(n))) = Bool.toString(n)^" "	
		|optree2string (NODE(n , args)) = 
		let
			val reconnode = ( List.foldr ((op^)) (" ") (List.map (optree2string) (args)) )
		in
			n^"("^reconnode^")"
		end

	(*given a substitution and a list of variables on which the sub must be applied, return the mapped values. *)
	fun tostr_sub (sub) ([]) (acc) = acc
		|tostr_sub (sub) (v::varlist) (acc) = tostr_sub (sub) (varlist) ( (optree2string v)^" |-> "^(optree2string (unif.substontree (unif.substfunc (sub)) (v)) )^" & "^acc)

	fun contains([],x) = false
	|contains(VARIABLE(x)::xs , VARIABLE(y)) = if(x=y) then true else contains(xs,VARIABLE(y))	

	fun make_set([]) (lis) = lis
		|make_set(x::xs) (lis) = if(contains(lis,x)) then make_set(xs) (lis) else (make_set (xs) (x::lis))	


	fun myhelper (NONVARIABLE(_)) (ans) = ans
		|myhelper (VARIABLE(x)) (ans) = ((VARIABLE(x))::(ans)) 
		|myhelper (NODE(x , treelist : optree list)) (ans) = 
								(List.foldl (op@) (ans) (List.map (vars) (treelist))) 
	and vars mytree = make_set (myhelper (mytree) ([] : optree list)) ([]);


	(* given a var list and a list of substitutions possible, return the string version. *)
	fun tostr_sublist (([]),(varlist),(acc)) = acc
		|tostr_sublist ((s::subl),(varlist),(acc)) = (tostr_sublist((subl),(varlist),(acc^(tostr_sub (s) (varlist) ("\n")))))
	fun get_all_vars (([]),(varlist)) = make_set(varlist) ([])
		|get_all_vars ((qi::qlist),(varlist)) = get_all_vars ((qlist),((vars(qi))@varlist))
	fun printdriver (x) = 
		let
		val sublists = InterpreterDriver(x); (* list of list of substitutions. *)
		fun  tostr_iter ([]) (_) (ans) = (ans) 
			|tostr_iter (_) ([]) (ans) = (ans) 
			|tostr_iter (FACT(_)::stmtlist) (sublist) (ans) = tostr_iter (stmtlist) (sublist) (ans)
			|tostr_iter (RULE(_,_)::stmtlist) (sublist) (ans) = tostr_iter (stmtlist) (sublist) (ans)
			|tostr_iter (GOAL(q)::stmtlist) (sb::sblist) (ans)= 
				(*sb is a list of substitutions. *)
				let
					val all_vars:(optree list) = (get_all_vars((q),([] : optree list)))
					val return_ans = if( List.null(sb) ) then "false\n" else "true\n"
				in
					if(List.null(all_vars)) 
					then 
						(tostr_iter(stmtlist)(sblist)(ans^(tostr_sublist((sb),(all_vars),( return_ans )))))
					else
						(tostr_iter(stmtlist)(sblist)(ans^(tostr_sublist((sb),(all_vars),("")))))
				end
	in
		print(tostr_iter (unwrap x) (sublists) (""))
	end


end