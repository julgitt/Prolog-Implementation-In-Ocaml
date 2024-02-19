val main_color : string -> string

val sub_color : string -> string

val sub2_color : string -> string

val print_error : exn -> unit

val print_true : unit -> unit

val print_false : unit -> unit

val view : Ast.term -> Ast.term

val print_terms : Ast.term list -> unit

val print_clauses : Ast.clause list -> unit

val print_results : (Ast.variable * Ast.substitution) list -> unit