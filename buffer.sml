(*  Function to output 

fun outputter()
*)


	fun subst (NODE(nvar , args)) (roe) = let val new_kids = (List.map (revcur (subst) (roe)) (args)) in NONVARIABLE(nvar, new_kids) end
	|subst (NONVARIABLE(nvar)) = 
	|subst (VARIABLE(var)) (roe) = (roe (VAR(var,nil)));

	(* -----for printing-------*)
	(* convert optree to strings. *)
	fun optree2string (VARIABLE(vn)) = vn^" "
		|optree2string (NONVARIABLE(INT(n))) = Int.toString(n)^" "
		|optree2string (NONVARIABLE(RATIONAL(n))) = Real.toString.toString(n)^" "
		|optree2string (NONVARIABLE(LCID(n))) = (n)^" "
		|optree2string (NONVARIABLE(BOOL(n))) = Bool.toString(n)^" "	
		|optree2string (NODE(n, args)) = 
		let
			val reconnode = ( List.foldr ((op^)) (" ") (List.map (optree2string) (args)) )
		in
			n^"("^reconnode^")"
		end

	fun vhelper (NONVARIABLE(_)) (ans) = ans
		|vhelper (VARIABLE(x)) (ans) = ((VARIABLE(x))::(ans)) 
		|vhelper (NODE(x , treelist : optree list)) (ans) = 
								(List.foldl (op@) (ans) (List.map (vars) (treelist))) 
	and vars mytree = vhelper (mytree) ([] : optree list);

	(*given a substitution and a list of variables on which the sub must be applied, return the mapped values. *)
	fun tostr_sub (sub) ([]) (acc) = acc
		|tostr_sub (sub) (v::varlist) (acc) = tostr_sub (sub) (varlist) ( (optree2string v)^" --> "^(optree2string (subst v sub) )^" & "^acc)

	(* given a var list and a list of substitutions possible, return the string version. *)
	fun tostr_sublist (([]),(varlist),(acc)) = acc
		|tostr_sublist ((s::subl),(varlist),(acc)) = (tostr_sublist((subl),(varlist),(acc^(tostr_sub (s) (varlist) ("\n")))))
(*)
	fun get_all_vars (([]),(varlist)) = make_set(varlist) ([])
		|get_all_vars ((qi::qlist),(varlist)) = get_all_vars ((qlist),((vars(qi))@varlist))
*)
	fun printdriver (x) = 
		let
		val sublists = driver(x); (* list of list of substitutions. *)
		fun  tostr_iter ([]) (_) (ans) = (ans) 
			|tostr_iter (_) ([]) (ans) = (ans) 
			|tostr_iter (FACT(_)::stmtlist) (sublist) (ans) = tostr_iter (stmtlist) (sublist) (ans)
			|tostr_iter (RULE(_,_)::stmtlist) (sublist) (ans) = tostr_iter (stmtlist) (sublist) (ans)
			|tostr_iter (GOAL(q)::stmtlist) (sb::sblist) (ans)= 
				(*sb is a list of substitutions. *)
				let
					val all_vars:(optree list) = (get_all_vars((q),([] : optree list)))
				in
					(tostr_iter(stmtlist)(sblist)(ans^(tostr_sublist((sb),(all_vars),("")))))
				end
	in
		print(tostr_iter (unwrap x) (sublists) (""))
	end



	fun uniq(VARIABLE(x))(VARIABLE(y)) = if(x=y) then false else true
		|uniq(VARIABLE(x))(NONVARIABLE(_)) = true 
		|uniq(VARIABLE(x))(NODE(a,b)) = (List.foldl (fn(p,q)=>p andalso q) true (List.map (uniq(VARIABLE(x))) b));





			|InterpreterDriverHelper(prog: Absyn.declaration list)(Absyn.FACT(_)::xs) = InterpreterDriverHelper(prog: Absyn.declaration list)(xs)
			|InterpreterDriverHelper(prog: Absyn.declaration list)(Absyn.RULE(_)::xs) = InterpreterDriverHelper(prog: Absyn.declaration list)(xs)




		(*) $ append(X , [] , X) . $ append([A|B] , C , [A|Y]) :- append(B,C,Y). ? append( [a,b] , M , [a,b,c]). ;";*)
