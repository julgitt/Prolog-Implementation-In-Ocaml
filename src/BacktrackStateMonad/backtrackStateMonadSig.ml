open Ast;;

module type BacktrackStateMonadSig = sig
  type state
  type 'a t
  
  (* Stack operations *)
  val is_empty : unit -> bool t 
  val get      : unit -> state t
  val push      : state -> unit t
  val pop      : unit -> state t 

  (* Clearing/Initialising *)
  val initialize  : clause list -> (variable * term option) list -> unit t

  (* State getters/setters *)
  val set_goals   : term list -> unit t
  val get_goals   : unit -> term list t

  val set_clauses   : clause list -> unit t
  val get_clauses   : unit -> clause list t
  val restore_clauses : unit -> unit t
  
  val set_substitutions : unit -> unit t 
  val get_substitutions : unit -> (variable * term option) list t
  val push_substitution : (variable * term option) -> bool t

  (* Backtracking *)
  val backtrack_goals : unit -> term list t
  
  (* Monad functions *)
  val return : 'a -> 'a t

  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  val (let+) : 'a t -> ('a -> 'b) -> 'b t

  val run : unit -> 'a t -> 'a
end