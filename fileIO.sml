
	(* ----for interpreter like behaviour-----*)
	val loaded = ref ([] : Absyn.declaration list) (* unwrapped program *)
	val load = fn ( PROGRAM(parsed_file) ) => loaded := (!loaded)@(parsed_file)
	val clearload = (fn()=> loaded := [])

	val interpret_loaded = (fn() =>InterpreterDriver (PROGRAM(!loaded)))
	val pinterpret_loaded = (fn() =>printdriver (PROGRAM(!loaded)))

	fun queryit (PROGRAM(s)) = (printdriver (PROGRAM((!loaded)@s)) ) ;
