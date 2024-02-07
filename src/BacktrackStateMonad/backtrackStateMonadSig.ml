module type BSM_Sig = sig
   type state
   type 'a t
   
   (* Stack operations *)
   val is_empty : unit -> bool t 
   val get      : unit -> state t
   val push      : state -> unit t
   val pop      : unit -> state t 

   (* Clearing/Initialising *)
   val initialize  : Ast.program -> 
                     (Ast.variable * Ast.substitution) list -> unit t

   (* State getters/setters *)
   val apply_substitutions : Ast.term list -> (Ast.term list) t
   val set_goals   : Ast.term list -> unit t
   val get_goals   : unit -> Ast.term list t

   val set_clauses   : Ast.clause list -> unit t
   val get_clauses   : unit -> Ast.clause list t
   val restore_clauses : unit -> unit t
   
   val set_substitutions : unit -> unit t 
   val get_substitutions : unit ->  (Ast.variable * Ast.substitution) list t
   val push_substitution : (Ast.variable * Ast.substitution) -> bool t

   (* Backtracking *)
   val backtrack_goals : unit -> Ast.term list t
   
   (* Monad functions *)
   val return : 'a -> 'a t

   val (let*) : 'a t -> ('a -> 'b t) -> 'b t
   val (let+) : 'a t -> ('a -> 'b) -> 'b t

   val run : unit -> 'a t -> 'a
end