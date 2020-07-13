type tokens =
	| Number of float
	| String of string
	| ID of string
	| Plus
	| Minus
	| Times
	| Divide
	| PlusEquals
	| MinusEquals
	| TimesEquals
	| DivideEquals
	| Power
	| Semicolon
	| Colon
	| LeftBrace
	| RightBrace
	| Percent
	| Whitespace
	| Indent
	| Comma
	| LeftSBrace
	| RightSBrace
	| LeftCBrace
	| RightCBrace
	| Dot
	| Define
	| Newline
	| Return
	| EOF
	| Assign
	| If
	| Elif
	| Else
	| Equals
	| GreaterThan
	| LessThan
	| GreaterThanEquals
	| LessThanEquals
	| For
	| In
	| True
	| False
	| And
	| Or
	| While
	| Not
	| Nonlocal
	| Global
	
let program = ref Bytes.empty

let () =
	let ic = open_in Sys.argv.(1) in
	try
		let n = in_channel_length ic in
		let s = Bytes.create n in
		really_input ic s 0 n;
		program := s;
		close_in ic
	with e ->
		close_in_noerr ic;
		raise e

let program = Bytes.to_string !program
		
let tokenise program =
	let tokens = ref [] in
	let index = ref 0 in
	let advance () =
		index := !index + 1;
		if !index > String.length program then
			raise (Failure "Reached EOF while tokenising!")
	in
	let devance () =
		index := !index - 1;
	in
	let current () =
		String.get program !index
	in
	let peek () =
		if !index + 1 < String.length program then
			String.get program (!index + 1)
		else
			raise (Failure "Cannot peak past EOF!")
	in
	
	let is_digit c =
		match c with
			'0'..'9' -> true
			| _ -> false
	in
	let is_alpha c =
		match c with 
			'a'..'z' -> true
			| 'A'..'Z' -> true
			| _ -> false
	in
	let is_alnum c =
		match c with
			'a'..'z' -> true
			| 'A'..'Z' -> true
			| '0'..'9' -> true
			| '_' -> true
			|  _  -> false
	in
	
	while !index < String.length program do
		(match (current ()) with 
			'0'..'9' -> (
				let number = ref 0. in
				while is_digit (current ()) do
					number := !number *. 10.;
					number := !number +. (float_of_string (String.make 1 (current ())));
					advance ()
				done;
				if current() = '.' then (
					advance ();
					let decimal_places = ref 0 in
					let decimal_number = ref 0. in
					while is_digit (current ()) do
						decimal_places := !decimal_places + 1;
						decimal_number := !decimal_number *. 10.;
						decimal_number := !decimal_number +. (float_of_string (String.make 1 (current ())));
						advance ()
					done;
					number := !number +. (!decimal_number /. (float !decimal_places))
				);
				devance ();
				tokens := !tokens @ [Number !number]
			)
			| ('a'..'z'|'A'..'Z'|'_') -> (
				let text = ref "" in
				while is_alnum (current ()) do
					text := !text ^ (String.make 1 (current ()));
					advance()
				done;
				devance ();
				match !text with
					| "def" -> tokens := !tokens @ [Define]
					| "return" -> tokens := !tokens @ [Return]
					| "if" -> tokens := !tokens @ [If]
					| "elif" -> tokens := !tokens @ [Elif]
					| "else" -> tokens := !tokens @ [Else]
					| "for" -> tokens := !tokens @ [For]
					| "in" -> tokens := !tokens @ [In]
					| "and" -> tokens := !tokens @ [And]
					| "or" -> tokens := !tokens @ [Or]
					| "True" -> tokens := !tokens @ [True]
					| "False" -> tokens := !tokens @ [False]
					| "not" -> tokens := !tokens @ [Not]
					| "while" -> tokens := !tokens @ [While]
					| "global" -> tokens := !tokens @ [Global]
					| "nonlocal" -> tokens := !tokens @ [Nonlocal]
					| _ -> tokens := !tokens @ [ID !text]
			)
			| '"' -> (
				let text = ref "" in
				advance ();
				while current () <> '"' do
					text := !text ^ (String.make 1 (current ()));
					advance ()
				done;
				tokens := !tokens @ [String !text]
			)
			| '\'' -> (
				let text = ref "" in
				advance ();
				while current () <> '\'' do
					text := !text ^ (String.make 1 (current ()))
				done;
				tokens := !tokens @ [String !text]
			)
			| '#' -> (
				while current () <> '\n' do
					advance ()
				done
			)
			| '=' -> (
				match peek () with
					'=' -> tokens := !tokens @ [Equals]; advance ()
					| _ -> tokens := !tokens @ [Assign]
			)
			| '>' -> (
				match peek () with
					'=' -> tokens := !tokens @ [GreaterThanEquals]; advance ()
					| _ -> tokens := !tokens @ [GreaterThan]
			)
			| '<' -> (
				match peek () with
					'=' -> tokens := !tokens @ [LessThanEquals]; advance ()
					| _ -> tokens := !tokens @ [LessThan]
			)
			| '+' -> (
				match peek () with 
					'=' -> tokens := !tokens @ [PlusEquals]; advance ()
					| _ -> tokens := !tokens @ [Plus]
			)
			| '-' -> (
				match peek () with 
					'=' -> tokens := !tokens @ [MinusEquals]; advance ()
					| _ -> tokens := !tokens @ [Minus]
			)
			| '*' -> (
				match peek () with 
					'=' -> tokens := !tokens @ [TimesEquals]; advance ()
					| '*' -> tokens := !tokens @ [Power]; advance ()
					| _ -> tokens := !tokens @ [Times]
			)
			| '/' -> (
				match peek () with 
					'=' -> tokens := !tokens @ [DivideEquals]; advance ()
					| _ -> tokens := !tokens @ [Divide]
			)
			| ' ' -> tokens := !tokens @ [Whitespace]
			| ';' -> tokens := !tokens @ [Semicolon]
			| ':' -> tokens := !tokens @ [Colon]
			| '.' -> tokens := !tokens @ [Dot]
			| ',' -> tokens := !tokens @ [Comma]
			| '%' -> tokens := !tokens @ [Percent]
			| '\n' -> tokens := !tokens @ [Newline]
			| '\t' -> tokens := !tokens @ [Indent]
			| '(' -> tokens := !tokens @ [LeftBrace]
			| ')' -> tokens := !tokens @ [RightBrace]
			| '[' -> tokens := !tokens @ [LeftSBrace]
			| ']' -> tokens := !tokens @ [RightSBrace]
			| '{' -> tokens := !tokens @ [LeftCBrace]
			| '}' -> tokens := !tokens @ [RightCBrace]
			| _ -> raise (Failure ("Invalid Character: " ^ (String.make 1 (current ()))))
		);
		advance ();
	done;
	tokens := !tokens @ [EOF];
	!tokens
	
let string_of_token tok =
	match tok with
		Plus -> "Plus"
		| Minus -> "Minus"
		| Times -> "Times"
		| Divide -> "Divide"
		| PlusEquals -> "PlusEquals"
		| MinusEquals -> "MinusEquals"
		| TimesEquals -> "TimesEquals"
		| DivideEquals -> "DivideEquals"
		| Power -> "Power"
		| Number n -> ("Number (" ^ string_of_float n ^ ")")
		| String s -> ("String (\"" ^ s ^ "\")")
		| ID id -> ("ID (" ^ id ^ ")")
		| Semicolon -> "Semicolon"
		| Colon -> "Colon"
		| LeftBrace -> "Left Bracket"
		| RightBrace -> "Right Bracket"
		| LeftSBrace -> "Left Square Bracket"
		| RightSBrace -> "Right Square Bracket"
		| LeftCBrace -> "Left Curly Bracket"
		| RightCBrace -> "Right Curly Bracket"
		| Comma -> "Comma"
		| Whitespace -> "Whitespace"
		| Dot -> "Dot"
		| Indent -> "Indent"
		| Percent -> "Percent"
		| Define -> "Define"
		| Return -> "Return"
		| Newline -> "Newline"
		| EOF -> "End of File"
		| Assign -> "Assign"
		| _ -> "NOT IMPLEMENTED YET"
		
let print_tokens tokens =
	for i = 0 to List.length tokens - 1 do
		print_endline ("Found: " ^ string_of_token (List.nth tokens i))
	done

type tree_operation =
	| NumberValue of float
	| StringValue of string
	| Variable of string
	| BoolValue of bool
	| DictValue of (tree_operation, tree_operation) Hashtbl.t
	| ListValue of tree_operation list
	| Compound of tree_operation list
	| Add of tree_operation * tree_operation
	| Sub of tree_operation * tree_operation
	| Mul of tree_operation * tree_operation
	| Div of tree_operation * tree_operation
	| Modulo of tree_operation * tree_operation
	| Pow of tree_operation * tree_operation
	| ItemOf of tree_operation * tree_operation
	| TestEquals of tree_operation * tree_operation
	| TestGreaterThan of tree_operation * tree_operation
	| TestLessThan of tree_operation * tree_operation
	| TestGreaterThanEquals of tree_operation * tree_operation
	| TestLessThanEquals of tree_operation * tree_operation
	| AndStatement of tree_operation * tree_operation
	| OrStatement of tree_operation * tree_operation
	| TestIn of tree_operation * tree_operation
	| NotStatement of tree_operation
	| IfStatement of tree_operation * tree_operation
	| WhileLoop of tree_operation * tree_operation
	| IfElseStatement of tree_operation * tree_operation * tree_operation
	| ForLoop of tree_operation * tree_operation * tree_operation
	| AssignVar of tree_operation * tree_operation
	| FunctionDeclaration of tree_operation * tree_operation * tree_operation
	| Action of tree_operation * tree_operation
	| RelativeAction of tree_operation * tree_operation * tree_operation
	| ArgList of tree_operation list
	| InputList of tree_operation list
	| Negate of tree_operation
	| PlaceHolder
	| Return of tree_operation
	| DefineGlobal of tree_operation
	| DefineNonlocal of tree_operation
	
let build_ast tokens =
	let index = ref 0 in
	
	let current () = 
		List.nth tokens !index
	in
	
	let eat token = 
		if current () = token then (
			index := !index + 1;
			print_endline ("Ate: " ^ string_of_token token)
		) else
			raise (Failure ("Invalid Token! Expected: " ^ string_of_token token ^ ", got: " ^ string_of_token (current ())))
	in
	let eat_number () =
		if (match current () with Number n -> true | _ -> false) then (
			index := !index + 1;
			(* print_endline "Ate: Number" *)
		) else
			raise (Failure ("Invalid Token! Expected: Number, got: " ^ string_of_token (current ())))
	in
	let eat_string () =
		if (match current () with String s -> true | _ -> false) then (
			index := !index + 1;
			(* print_endline "Ate: String" *)
		) else
			raise (Failure ("Invalid Token! Expected: String, got: " ^ string_of_token (current ())))
	in
	let eat_id () =
		if (match current () with ID id -> true | _ -> false) then (
			index := !index + 1;
			(* print_endline "Ate: ID" *)
		) else
			raise (Failure ("Invalid Token! Expected: ID, got: " ^ string_of_token (current ())))
	in
	let eat_statement_separator () =
		(match current () with
			Newline -> eat Newline
			| EOF -> ()
			| _ -> eat Semicolon);
		while current () = Newline do
			eat Newline
		done
	in
	let eat_whitespace () =
		while current () = Whitespace do
			eat Whitespace
		done
	in
	
	let list_of_compound c =
		match c with 
			Compound c -> c
			| _ -> raise (Failure "Not a Compound")
	in
	let list_of_arglist al =
		match al with 
			ArgList al -> al
			| _ -> raise (Failure "Not a ArgList")
	in
	let list_of_inputlist il =
		match il with 
			InputList il -> il
			| _ -> raise (Failure "Not a ArgList")
	in
	
	let string_of_id tok =
		match tok with
			ID id -> id
			| _ -> raise (Failure "Not an ID")
	in
	
	
	let eat_indents () =
		let found_indents = ref 0 in
		while current () = Indent do
			eat Indent;
			found_indents := !found_indents + 1
		done;
		!found_indents
	in
	let devance ?(by=1) () =
		index := !index - by
	in
	
	let rec variable () =
		eat_whitespace ();
		print_endline (string_of_token (current ()));
		let node = ref (Variable (string_of_id (current ()))) in
		eat_id ();
		!node
	and factor () =
		let node = ref PlaceHolder in
		eat_whitespace ();
		if (match current () with Number n -> true | _ -> false) then (
			node := NumberValue (match current() with Number n -> n | _ -> 0.);
			eat_number ();
		) else if (match current () with String s -> true | _ -> false) then (
			node := StringValue (match current() with String s -> s | _ -> "");
			eat_string ();
		) else if current() = True then (
			node := BoolValue true;
			eat True
		) else if current() = False then (
			node := BoolValue false;
			eat False
		) else if current () = Minus then (
			eat Minus;
			node := Negate ( factor () );
			eat_whitespace ()
		) else if current () = LeftBrace then (
			eat LeftBrace;
			eat_whitespace ();
			node := expr ();
			eat_whitespace ();
			eat RightBrace
		) else if current () = LeftSBrace then (
			eat LeftSBrace;
			eat_whitespace ();
			let tmp_list = ref [] in
			if current () <> RightSBrace then (
				tmp_list := !tmp_list @ [expr ()];
				while current () = Comma do
					eat Comma;
					tmp_list := !tmp_list @ [expr ()];
				done
			);
			eat RightSBrace;
			node := ListValue !tmp_list
		) else if current () = LeftCBrace then (
			eat LeftCBrace;
			eat_whitespace;
			let tmp_Hashtbl = Hashtbl.create 100 in
			let tmp_key = ref (factor ()) in
			eat Colon;
			let tmp_value = ref (factor ()) in
			Hashtbl.replace tmp_Hashtbl !tmp_key !tmp_value;
			while current () = Comma do
				eat Comma;
				let tmp_key = ref (factor ()) in
				eat Colon;
				let tmp_value = ref (factor ()) in
				Hashtbl.replace tmp_Hashtbl !tmp_key !tmp_value;
			done;
			node := DictValue tmp_Hashtbl
		) else (
			node := variable ();
			eat_whitespace ();
			if current () = LeftBrace then
				node := Action( !node, action () )
		);
		while current() = LeftSBrace do
			eat LeftSBrace;
			node := ItemOf ( !node, factor () );
			eat RightSBrace
		done;
		if current () = Dot then (
			eat Dot;
			let var = variable () in
			node := RelativeAction (var, !node, action ())
		);
		eat_whitespace ();
		!node
	and power () =
		let node = ref (factor ()) in
		while current () = Power do
			eat Power;
			node := Pow (!node, factor ())
		done;
		!node
	and term () =
		let node = ref (power ()) in
		while (match current () with (Times|Divide|Percent) -> true | _ -> false) do
			if current () = Times then (
				eat Times;
				node := Mul(!node, power ())
			) else  if current () = Divide then (
				eat Divide;
				node := Div(!node, power ())
			) else (
				eat Percent;
				node := Modulo(!node, power ())
			)
		done;
		!node
	and expr () =
		let node = ref (term ()) in
		while (match current () with (Plus|Minus) -> true | _ -> false) do
			if current () = Plus then (
				eat Plus;
				node := Add(!node, term ())
			) else (
				eat Minus;
				node := Sub(!node, term ())
			)
		done;
		!node
	and function_def ?(indents=0) () =
		let node = ref PlaceHolder in
		eat Define;
		eat_whitespace ();
		let name = variable () in
		eat_whitespace ();
		eat LeftBrace;
		let inputs = ref (InputList []) in
		if (match current () with ID id -> true | _ -> false) then (
			inputs := InputList ((list_of_inputlist !inputs) @ [variable ()]);
			while current () = Comma do
				eat Comma;
				eat_whitespace ();
				inputs := InputList ((list_of_inputlist !inputs) @ [variable ()]);
			done
		);
		eat RightBrace;
		eat Colon;
		eat_whitespace ();
		eat Newline;
		let to_exec = statement_list ~indents:(indents+1) () in
		node := FunctionDeclaration(name, !inputs, to_exec);
		!node
	and action () =
		let node = ref (ArgList []) in
		eat_whitespace ();
		eat LeftBrace;
		if current () <> RightBrace then (
			node := ArgList ((list_of_arglist !node) @ [expr ()]);
			eat_whitespace ();
			while current () = Comma do
				eat Comma;
				eat_whitespace ();
				node := ArgList ((list_of_arglist !node) @ [expr ()])
			done
		);
		eat RightBrace;
		!node
	and assignment () =
		let node = ref PlaceHolder in
		eat_whitespace ();
		eat Assign;
		node := expr ();
		!node
	and assign_action () =
		let node = ref (variable ()) in
		eat_whitespace ();
		(match current () with
			Assign -> node := AssignVar (!node, assignment ())
			| PlusEquals -> (
				eat PlusEquals;
				eat_whitespace ();
				node := AssignVar (!node, Add ( !node, expr () ))
			)
			| MinusEquals -> (
				eat MinusEquals;
				eat_whitespace ();
				node := AssignVar (!node, Sub ( !node, expr () ))
			)
			| TimesEquals -> (
				eat TimesEquals;
				eat_whitespace ();
				node := AssignVar (!node, Mul ( !node, expr () ))
			)
			| DivideEquals -> (
				eat DivideEquals;
				eat_whitespace ();
				node := AssignVar (!node, Div ( !node, expr () ))
			)
			| Dot -> (
				eat Dot;
				let var = variable () in
				node := RelativeAction(var, !node, action ())
			)
			| _ -> node := Action (!node, action ()));
		!node
	and base_equiv () =
		let node = ref PlaceHolder in
		eat_whitespace ();
		if current () = Not then (
			eat Not;
			node := NotStatement ( base_equiv () );
			!node
		) else (
			node := factor ();
			(match current () with
				Equals -> eat Equals; node := TestEquals ( !node, factor () )
				| GreaterThan -> eat GreaterThan; node := TestGreaterThan ( !node, factor () )
				| LessThan -> eat LessThan; node := TestLessThan ( !node, factor () )
				| GreaterThanEquals -> eat GreaterThanEquals; node := TestGreaterThanEquals ( !node, factor () )
				| In -> eat In; node := TestIn (!node, factor ())
				| LessThanEquals -> eat LessThanEquals; node := TestLessThanEquals ( !node, factor () )
				| _ -> ());
			!node
		)
	and equiv_expr () =
		let node = ref (base_equiv ()) in
		while (match current () with (And|Or) -> true | _ -> false) do
			if current () = And then (
				eat And;
				node := AndStatement (!node, base_equiv ())
			) else (
				eat Or;
				node := OrStatement (!node, base_equiv ())
			)
		done;
		!node
	and statement ?(indents=0) () =
		let node = ref PlaceHolder in
		
		let found_indents = eat_indents () in
		
		let rec if_statement () =
			let if_comp = equiv_expr () in 
			eat Colon;
			let if_body = statement_list ~indents:(indents+1) () in
			let else_body = ref PlaceHolder in
			
			let found_indents = eat_indents () in
			if found_indents = indents then (
				if current () = Else then (
					eat Else;
					eat Colon;
					else_body := statement_list ~indents:(indents+1) ()
				) else if current () = Elif then (
					eat Elif;
					else_body := if_statement ()
				);
			) else (
				devance ~by:found_indents ()
			);
			if !else_body = PlaceHolder then
				IfStatement (if_comp, if_body)
			else
			IfElseStatement (if_comp, if_body, !else_body)
		in
		
		if found_indents = indents then (
			(match current () with 
				ID id -> (
					node := assign_action ();
					eat_statement_separator ()
				)
				| Return -> (
					eat Return;
					node := Return (expr ());
					eat_statement_separator ()
				)
				| If -> (
					eat If;
					node := if_statement ()
				)
				| While -> (
					eat While;
					let while_comp = equiv_expr () in
					eat Colon;
					let while_body = statement_list ~indents:(indents+1) () in
					node := WhileLoop (while_comp, while_body)
				)
				| For -> (
					eat For;
					eat_whitespace ();
					let var = variable () in
					eat_whitespace ();
					eat In;
					let for_expr = factor () in
					eat Colon;
					let for_body = statement_list ~indents:(indents+1) () in
					node := ForLoop (var, for_expr, for_body)
				)
				| Global -> (
					eat Global;
					node := DefineGlobal (variable ());
					eat_statement_separator ()
				)
				| Nonlocal -> (
					eat Nonlocal;
					node := DefineNonlocal (variable ());
					eat_statement_separator ()
				)
				| _ -> node := function_def ~indents:(indents) ()
			)
		) else (
			devance ~by:found_indents ()
		);
		!node
	and statement_list ?(indents = 0) () =
		if current () = Newline then
			eat_statement_separator ();
		
		let node = ref (Compound [statement ~indents:indents ()]) in
		
		let found_indents = ref (eat_indents ()) in
		
		while (!found_indents = indents && (match current () with ID id -> true | (Return|If|Define|For|While) -> true | _ -> false)) do
			devance ~by:!found_indents ();
			node := Compound ((list_of_compound !node) @ [statement ~indents:indents ()]);
			found_indents := eat_indents ();
			eat_whitespace ()
		done;
		devance ~by:!found_indents ();
		!node
	and program () =
		let node = ref (statement_list ()) in
		eat_statement_separator ();
		eat EOF;
		!node
	in
	program ()
	
let print_ast ast =
	let rec string_of_ast ast =
		match ast with
			NumberValue n -> ("(Number: " ^ string_of_float n ^ ")")
			| StringValue s -> ("String: " ^ s ^ ")")
			| Variable v -> ("(Variable: " ^ v ^ ")")
			| Compound c -> (
				let tmp = ref "" in
				for i = 0 to List.length c - 1 do
					tmp := (!tmp ^ string_of_ast (List.nth c i) ^ ", ") 
				done;
				tmp := "(Compoud: " ^ !tmp ^ ")";
				!tmp
			)
			| Add (left, right) -> ("(" ^ string_of_ast left ^ " + " ^ string_of_ast right ^ ")")
			| Sub (left, right) -> ("(" ^ string_of_ast left ^ " - " ^ string_of_ast right ^ ")")
			| Mul (left, right) -> ("(" ^ string_of_ast left ^ " * " ^ string_of_ast right ^ ")")
			| Div (left, right) -> ("(" ^ string_of_ast left ^ " / " ^ string_of_ast right ^ ")")
			| AssignVar (var, value) -> ("(Assign " ^ string_of_ast var ^ " to " ^ string_of_ast value ^ ")")
			| FunctionDeclaration (name, inputs, to_exec) -> ("(Create Function " ^ string_of_ast name ^ " with the inputs " ^ string_of_ast inputs ^ " which runs the code " ^ string_of_ast to_exec ^ ")")
			| Action (name, args) -> ("(Run the Function " ^ string_of_ast name ^ " with the arguments " ^ string_of_ast args ^ ")")
			| ArgList al -> (
				let tmp = ref "" in
				for i = 0 to List.length al - 1 do
					tmp := (!tmp ^ string_of_ast (List.nth al i) ^ ", ") 
				done;
				tmp := "(ArgList: " ^ !tmp ^ ")";
				!tmp
			)
			| InputList il -> (
				let tmp = ref "" in
				for i = 0 to List.length il - 1 do
					tmp := (!tmp ^ string_of_ast (List.nth il i) ^ ", ") 
				done;
				tmp := "(InputList: " ^ !tmp ^ ")";
				!tmp
			)
			| Negate t -> ("(- " ^ string_of_ast t ^ ")")
			| PlaceHolder -> "(PlaceHolder, if this is displaying something has gone horribly wrong!!!)"
			| Return t -> ("(Return: " ^ string_of_ast t ^ ")")
			| _ -> "(UNIMPLEMENTED FUNCTION PLEASE IMPLEMENT)"
	in
	print_endline (string_of_ast ast)

type value_wrapper =
	| Num of float
	| Str of string
	| Bool of bool
	| List of value_wrapper list
	| Dict of (value_wrapper, value_wrapper) Hashtbl.t
	| Nothing
	
let interpret ast= 
	let variables = Hashtbl.create 100 in
	let (functions: (tree_operation, tree_operation) Hashtbl.t) = Hashtbl.create 100 in
	let (functions_input: (tree_operation, tree_operation) Hashtbl.t) = Hashtbl.create 100 in
	
	let get_var var =
		if Hashtbl.mem variables var then
			Hashtbl.find variables var
		else
			raise (Failure "Reference to undefined variables")
	in
	
	let rmod a b = (a mod b + b) mod b in
	
	let set_function funcs funcinputs func inputs value=
		if List.length funcs = 1 then (
			if Hashtbl.mem functions func <> true && Hashtbl.mem variables func <> true then (
				Hashtbl.replace functions func value;
				Hashtbl.replace functions_input func inputs
			) else
				raise (Failure "Collision between function name and function name or function name and variable name")
		) else (
			let collision = ref false in
			for i = 0 to List.length funcs - 1 do
				if Hashtbl.mem (List.nth funcs i) func = true then
					collision := true
			done;
			if Hashtbl.mem functions func = true && Hashtbl.mem variables func = true then
				collision := true;
			if !collision = true then
				raise (Failure "Collision between function name and function name or function name and variable name")
			else (
				Hashtbl.replace (List.nth funcs 0) func value;
				Hashtbl.replace (List.nth funcinputs 0) func inputs
			)
		)
	in
	
	let float_of_num num =
		match num with
			Num n -> n
			| _ -> raise (Failure "Not a Num")
	in
	let string_of_str str =
		match str with
			Str s -> s
			| _ -> raise (Failure "Not a Str")
	in
	
	let list_of_arglist al =
		match al with 
			ArgList al -> al
			| _ -> raise (Failure "Not a ArgList")
	in
	let list_of_inputlist il =
		match il with 
			InputList il -> il
			| _ -> raise (Failure "Not a ArgList")
	in
	let name_of_variable v =
		match v with
			Variable v -> v
			| _ -> raise (Failure "Not a Variable")
	in
	let list_of_list l =
		match l with
			List l -> l
			| _ -> raise (Failure "Not a List")
	in
	
	let return_stack = ref [] in
	
	let string_of_valuewrapper wrapper =
		match wrapper with
				| Num n -> ("Num: " ^ string_of_float n)
				| Str s -> ("Str: " ^ s)
				| Nothing -> "Nothing"
	in
	
	let print_return_stack () =
		for i = 0 to List.length !return_stack - 1 do
			print_endline (string_of_valuewrapper (List.nth !return_stack i))
		done
	in
	
	let is_digit c =
		match c with
			'0'..'9' -> true
			| _ -> false
	in
	let is_alpha c =
		match c with 
			'a'..'z' -> true
			| 'A'..'Z' -> true
			| _ -> false
	in
	let is_alnum c =
		match c with
			'a'..'z' -> true
			| 'A'..'Z' -> true
			| '0'..'9' -> true
			| '_' -> true
			|  _  -> false
	in

	let (empty_vars_table: (tree_operation, value_wrapper) Hashtbl.t) = Hashtbl.create 0 in
	let (empty_funcs_table(*: (tree_operation, tree_operation) Hashtbl.t*)) = Hashtbl.create 0 in
	let (empty_funcinputs_table: (tree_operation, tree_operation) Hashtbl.t) = Hashtbl.create 0 in
	let empty_globals = ref [[]] in
	let empty_nonlocals = ref [[]] in
	
	let rec set_var var value vars globals nonlocals funcs funcinputs=
		let found = ref false in
		if List.mem var (List.nth !nonlocals 0) || List.mem var (List.nth !globals 0) then (
			for i = 0 to List.length vars - 1 do
				if Hashtbl.mem (List.nth vars i) (var) then (
					found := true;
					Hashtbl.replace (List.nth vars i) var value
				)
			done
		) else (
			found := true;
			Hashtbl.replace (List.nth vars 0) var value
		);
		
		if !found <> true then
			if List.mem var (List.nth !globals 0) then
				Hashtbl.replace variables var value
			else if List.length !globals = 1 && List.length !nonlocals = 1 then
				Hashtbl.replace variables var value
			else
				Hashtbl.replace (List.nth vars 0) var value
				(*raise (Failure ("Could not assign variable: " ^ name_of_variable var ^ "! It does not exist within this scope"));*)
	and eval vars globals nonlocals funcs funcinputs ast =
		match ast with
			| DefineGlobal v -> (
				let tmp_globals = ref !globals in
				let tmp_item = List.nth !tmp_globals 0 in
				tmp_globals := List.tl !tmp_globals;
				globals := [tmp_item @ [v]] @ !tmp_globals;
				Nothing
			)
			| DefineNonlocal v -> (
				let tmp_nonlocals = ref !nonlocals in
				let tmp_item = List.nth !tmp_nonlocals 0 in
				tmp_nonlocals := List.tl !tmp_nonlocals;
				nonlocals := [tmp_item @ [v]] @ !tmp_nonlocals;
				Nothing
			)
			| NumberValue nv -> Num nv
			| StringValue sv -> Str sv
			| BoolValue bv -> Bool bv
			| DictValue dv -> (
				let tmp_hashtbl = Hashtbl.create 100 in
				Hashtbl.iter (fun k v -> Hashtbl.replace tmp_hashtbl (eval vars globals nonlocals funcs funcinputs k) (eval vars globals nonlocals funcs funcinputs v)) dv;
				Dict tmp_hashtbl
			)
			| ListValue lv -> (
				let tmp_list = ref [] in
				for i = 0 to List.length lv - 1 do
					tmp_list := !tmp_list @ [eval vars globals nonlocals funcs funcinputs (List.nth lv i)]
				done;
				List !tmp_list
			)
			| Variable v -> (
				let found = ref false in
				let returnable = ref Nothing in
				for i = 0 to List.length vars - 1 do
					if Hashtbl.mem (List.nth vars i) (Variable v) then (
						found := true;
						returnable := Hashtbl.find (List.nth vars i) (Variable v)
					)
				done;
				if !found <> true then
					returnable := get_var (Variable v);
				!returnable
			)
			| Compound c -> (
				for i = 0 to List.length c - 1 do
					eval vars globals nonlocals funcs funcinputs (List.nth c i)
				done;
				Nothing
			)
			| Add (left, right) -> (
				match eval vars globals nonlocals funcs funcinputs left with
					Num n -> Num (float_of_num (eval vars globals nonlocals funcs funcinputs left) +. float_of_num (eval vars globals nonlocals funcs funcinputs right))
					| Str s -> Str (string_of_str (eval vars globals nonlocals funcs funcinputs left) ^ string_of_str (eval vars globals nonlocals funcs funcinputs right))
			)
			| Sub (left, right) -> Num (float_of_num (eval vars globals nonlocals funcs funcinputs left) -. float_of_num (eval vars globals nonlocals funcs funcinputs right))
			| Mul (left, right) -> (
				match eval vars globals nonlocals funcs funcinputs left with
					Num n -> Num (float_of_num (eval vars globals nonlocals funcs funcinputs left) *. float_of_num (eval vars globals nonlocals funcs funcinputs right))
					| Str s -> (
						let tmp_str = ref "" in
						let tmp_left = string_of_str (eval vars globals nonlocals funcs funcinputs left) in
						for i = 0 to int_of_float (float_of_num (eval vars globals nonlocals funcs funcinputs right)) - 1 do
							tmp_str := !tmp_str ^ tmp_left
						done;
						Str !tmp_str
					)
			)
			| Pow (left, right) -> Num (float_of_num (eval vars globals nonlocals funcs funcinputs left) ** float_of_num (eval vars globals nonlocals funcs funcinputs right))
			| Div (left, right) -> Num (float_of_num (eval vars globals nonlocals funcs funcinputs left) /. float_of_num (eval vars globals nonlocals funcs funcinputs right))
			| AssignVar (var, value) -> (
				let tmp_nonlocals = ref !nonlocals in
				let tmp_item = List.nth !tmp_nonlocals 0 in
				tmp_nonlocals := List.tl !tmp_nonlocals;
				nonlocals := [tmp_item @ [var]] @ !tmp_nonlocals;
				set_var var (eval vars globals nonlocals funcs funcinputs value) vars globals nonlocals funcs funcinputs;
				Nothing
			)
			| FunctionDeclaration (func, inputs, to_exec) -> (
				set_function funcs funcinputs func inputs to_exec;
				Nothing
			)
			| Action (func, args) -> (
				match name_of_variable func with
					"print" -> (
						let tmp_str = ref "" in
						let tmp_args = list_of_arglist args in
						for i = 0 to List.length tmp_args - 1 do
							tmp_str := !tmp_str ^ (string_of_str (eval vars globals nonlocals funcs funcinputs (List.nth tmp_args i))) ^ " "
						done;
						print_endline !tmp_str;
						Nothing
					)
					| "input" -> (
						let returnable = ref Nothing in
						let tmp_args = list_of_arglist args in
						if List.length tmp_args = 1 then (
							print_string (string_of_str (eval vars globals nonlocals funcs funcinputs (List.nth tmp_args 0)));
							returnable := Str (read_line ())
						) else (
							raise (Failure "Too many arguments for input()")
						);
						!returnable
					)
					| "str" -> (
						let returnable = ref Nothing in
						let tmp_args = list_of_arglist args in
						if List.length tmp_args = 1 then (
							let tmp_return = eval vars globals nonlocals funcs funcinputs (List.nth tmp_args 0) in
							match tmp_return with
								Num n -> returnable := Str (string_of_float (n))
								| Str s -> returnable := tmp_return
						) else (
							raise (Failure "Too many arguments for str()")
						);
						!returnable
					)
					| "len" -> (
						let returnable = ref Nothing in
						let tmp_args = list_of_arglist args in
						if List.length tmp_args = 1 then (
							let tmp_return = eval vars globals nonlocals funcs funcinputs (List.nth tmp_args 0) in
							match tmp_return with
								Str s -> returnable := Num (float_of_int (String.length s))
						) else (
							raise (Failure "Too many arguments for len()")
						);
						!returnable
					)
					| "int" -> (
						let returnable = ref Nothing in
						let tmp_args = list_of_arglist args in
						if List.length tmp_args = 1 then (
							let tmp_return = eval vars globals nonlocals funcs funcinputs (List.nth tmp_args 0) in
							match tmp_return with
								Str s -> returnable := Num (float_of_string s)
								| Num n -> returnable := tmp_return
						) else (
							raise (Failure "Too many arguments for int()")
						);
						!returnable
					)
					| "range" -> (
						let tmp_list = ref (List []) in
						let tmp_args = list_of_arglist args in
						if List.length tmp_args <> 2 then 
							raise (Failure "Too many / not enough arguments for range()");
						let left = int_of_float (float_of_num ( eval vars globals nonlocals funcs funcinputs (List.nth tmp_args 0))) in
						let right = int_of_float (float_of_num ( eval vars globals nonlocals funcs funcinputs (List.nth tmp_args 1))) in
						for i = left to right - 1 do
							tmp_list := List (list_of_list !tmp_list @ [Num (float i)])
						done;
						!tmp_list
					)
					| "chr" -> (
						let tmp_args = list_of_arglist args in
						if List.length tmp_args <> 1 then
							raise (Failure "Too many / not enough arguments for chr()");
						Str (String.make 1 (char_of_int (int_of_float (float_of_num (eval vars globals nonlocals funcs funcinputs (List.nth tmp_args 0))))))
					)
					| "ord" -> (
						let tmp_args = list_of_arglist args in
						if List.length tmp_args <> 1 then
							raise (Failure "Too many / not enough arguments for chr()");
						Num (float (int_of_char (string_of_str (eval vars globals nonlocals funcs funcinputs (List.nth tmp_args 0))).[0]))
					)
					| _ -> (
						let returnable = ref Nothing in
						let function_call = ref PlaceHolder in
						let function_call_input = ref PlaceHolder in
						for i = 0 to List.length funcs - 1 do
							if Hashtbl.mem (List.nth funcs i) func then (
								function_call := Hashtbl.find (List.nth funcs i) func;
								function_call_input := Hashtbl.find (List.nth funcinputs i) func
							)
						done;
						if !function_call = PlaceHolder then
							if Hashtbl.mem functions func then (
								function_call := Hashtbl.find functions func;
								function_call_input := Hashtbl.find functions_input func
							);
						if !function_call <> PlaceHolder then (
							let arglist = list_of_arglist args in
							let inputlist = list_of_inputlist !function_call_input in
							if List.length arglist = List.length inputlist then (
								let func_vars = Hashtbl.copy (List.nth vars 0) in
								let func_funcs = Hashtbl.copy (List.nth funcs 0) in
								let func_funcinputs = Hashtbl.copy (List.nth funcinputs 0) in
								for i = 0 to List.length inputlist - 1 do
									Hashtbl.replace func_vars (List.nth inputlist i) (eval vars globals nonlocals funcs funcinputs (List.nth arglist i))
								done;
								returnable := eval ([func_vars] @ vars) (ref ([[]] @ !globals)) (ref ([[]] @ !nonlocals)) ([func_funcs] @ funcs) ([func_funcinputs] @ funcinputs) !function_call;
								if List.length !return_stack >= 1 then (
									returnable := (List.nth !return_stack 0);
									return_stack := List.tl !return_stack;
								);
								!returnable
							) else
								raise (Failure ("Too many / too little args for function: " ^ name_of_variable func))
						) else
							raise (Failure ("Undefined Function: " ^ name_of_variable func)) 
						!returnable
					)
			)
			| RelativeAction (func, actor, args) -> (
				match name_of_variable func with
					"isalpha" -> (
						let returnable = ref true in
						let tmp_actor = string_of_str (eval vars globals nonlocals funcs funcinputs actor) in
						for i = 0 to String.length tmp_actor - 1 do
							if is_alpha tmp_actor.[i] = false then
								returnable := false
						done;
						Bool !returnable
					)
					| "append" -> (
						set_var actor (List (list_of_list (eval vars globals nonlocals funcs funcinputs actor) @ [(eval vars globals nonlocals funcs funcinputs (List.nth (list_of_arglist args) 0))])) vars globals nonlocals funcs funcinputs;
						Nothing
					)
					| "isdigit" -> (
						let returnable = ref true in
						let tmp_actor = string_of_str (eval vars globals nonlocals funcs funcinputs actor) in
						for i = 0 to String.length tmp_actor - 1 do
							if is_digit tmp_actor.[i] = false then
								returnable := false
						done;
						Bool !returnable
					)
					| "isalnum" -> (
						let returnable = ref true in
						let tmp_actor = string_of_str (eval vars globals nonlocals funcs funcinputs actor) in
						for i = 0 to String.length tmp_actor - 1 do
							if is_alnum tmp_actor.[i] = false then
								returnable := false
						done;
						Bool !returnable
					)
			)
			| Negate t -> Num (-1. *. float_of_num (eval vars globals nonlocals funcs funcinputs t))
			| Return t -> (
				return_stack := [eval vars globals nonlocals funcs funcinputs t] @ !return_stack;
				Nothing
			)
			| TestEquals (left, right) -> Bool (eval vars globals nonlocals funcs funcinputs left = eval vars globals nonlocals funcs funcinputs right)
			| TestGreaterThan (left, right) -> (
				if float_of_num (eval vars globals nonlocals funcs funcinputs left) > float_of_num (eval vars globals nonlocals funcs funcinputs right) then
					Bool true
				else
					Bool false
			)
			| TestLessThan (left, right) -> (
				if float_of_num (eval vars globals nonlocals funcs funcinputs left) < float_of_num (eval vars globals nonlocals funcs funcinputs right) then
					Bool true
				else
					Bool false
			)
			| TestGreaterThanEquals (left, right) -> (
				if float_of_num (eval vars globals nonlocals funcs funcinputs left) >= float_of_num (eval vars globals nonlocals funcs funcinputs right) then
					Bool true
				else
					Bool false
			)
			| TestLessThanEquals (left, right) -> (
				if float_of_num (eval vars globals nonlocals funcs funcinputs left) <= float_of_num (eval vars globals nonlocals funcs funcinputs right) then
					Bool true
				else
					Bool false
			)
			| TestIn (left, right) -> (
				let tmp_left = eval vars globals nonlocals funcs funcinputs left in
				let tmp_right = eval vars globals nonlocals funcs funcinputs right in
				let tmp_list = list_of_list tmp_right in
				let found = ref false in
				for i = 0 to List.length tmp_list - 1 do
					if tmp_left = List.nth tmp_list i then
						found := true
				done;
				Bool !found
			)
			| AndStatement (left, right) -> Bool (eval vars globals nonlocals funcs funcinputs left = Bool true && eval vars globals nonlocals funcs funcinputs right = Bool true)
			| OrStatement (left, right) -> Bool (eval vars globals nonlocals funcs funcinputs left = Bool true || eval vars globals nonlocals funcs funcinputs right = Bool true)
			| IfStatement (test, func) -> (
				if (eval vars globals nonlocals funcs funcinputs test) = Bool true then
					eval vars globals nonlocals funcs funcinputs func
				else
					Nothing
			)
			| NotStatement t -> (
				match eval vars globals nonlocals funcs funcinputs t with
					Bool true -> Bool false
					| Bool false -> Bool true
			)
			| IfElseStatement (test, true_func, false_func) -> (
				if (eval vars globals nonlocals funcs funcinputs test) = Bool true then
					eval vars globals nonlocals funcs funcinputs true_func
				else
					eval vars globals nonlocals funcs funcinputs false_func;
				Nothing
			)
			| ForLoop (variable, for_expr, for_body) -> (
				let tmp_expr = eval vars globals nonlocals funcs funcinputs for_expr in
				(match tmp_expr with
					List l -> (
						let func_vars = Hashtbl.copy (List.nth vars 0) in
						for i = 0 to List.length l - 1 do
							Hashtbl.replace func_vars variable (List.nth l i);
							eval ([func_vars] @ vars) globals nonlocals funcs funcinputs for_body
						done;
					)
					| Str s -> (
						let func_vars = Hashtbl.copy (List.nth vars 0) in
						for i = 0 to String.length s - 1 do
							Hashtbl.replace func_vars variable (Str (String.make 1 s.[i]));
							eval ([func_vars] @ vars) globals nonlocals funcs funcinputs for_body
						done;
					)
				);
				
				Nothing
			)
			| WhileLoop (comparitor, body) -> (
				while eval vars globals nonlocals funcs funcinputs comparitor = Bool true do
					eval vars globals nonlocals funcs funcinputs body;
				done;
				Nothing
			)
			| ItemOf (item, index) -> (
				let returnable = ref Nothing in
				(match eval vars globals nonlocals funcs funcinputs item with
					Str s -> returnable := Str (String.make 1 (s).[int_of_float (float_of_num (eval vars globals nonlocals funcs funcinputs index))])
					| List l -> returnable := List.nth (l) (int_of_float (float_of_num (eval vars globals nonlocals funcs funcinputs index)))
					| Dict d -> returnable := (Hashtbl.find d (eval vars globals nonlocals funcs funcinputs index))
					| _ -> raise (Failure "Cannot use ItemOf on this"));
				!returnable
			)
			| Modulo (left, right) -> (
				let tmp_left = eval vars globals nonlocals funcs funcinputs left in
				let tmp_right = eval vars globals nonlocals funcs funcinputs right in
				Num (float (rmod (int_of_float (float_of_num tmp_left)) (int_of_float (float_of_num tmp_right))))
			)
			| _ -> raise (Failure "Executed BAD AST")
	in
	eval [empty_vars_table] empty_globals empty_nonlocals [empty_funcs_table] [empty_funcinputs_table] ast
	
let tokens = tokenise program;;
let ast = build_ast tokens;;
interpret ast;