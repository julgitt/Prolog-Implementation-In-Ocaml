open Ast
open Printf

let rec view t =
   match t with
   | Var (_, x) -> ( 
      match x with
      | None -> t
      | Some t -> view t
      )
   | _ -> t

let print_var s =
   printf "\x1b[38;5;173m%s\x1b[0m" s

let print_symbol s =
   printf "\x1b[38;5;221m%s\x1b[0m" s

let print_error error =
   printf "\x1b[1;31m%s\x1b[0m\n" (Errors.string_of_error error)

let print_true () = 
   print_endline "\x1b[38;5;40mtrue\x1b[0m.\n"

let print_false () = 
   print_endline "\x1b[38;5;196mfalse\x1b[0m.\n"

let rec print_terms q =
   match q with
   | [] -> ()
   | [t] -> (
      match view t with
      | Var(name, _) -> print_var name
      | Sym(name, []) -> print_symbol name
      | Sym(name, xs) -> 
         print_symbol name;
         printf "(";
         print_terms xs;
         printf ")"
      | Num x -> printf "%d" x;)
   | t :: ts  -> 
      print_terms [t];
      printf ", ";
      print_terms ts

let rec print_clauses p =
   match p with
   | [] -> ()
   | c :: cs -> ( 
      match c with 
      | Fact t -> print_terms [t]
      | Rule (t, ts) -> 
         print_terms [t];
         printf " :- ";
         print_terms ts
         );
      printf ".\n";
      print_clauses cs
    
let rec print_results solutions = 
   match solutions with
   | [] -> printf "";
   | [(name, v)] -> 
      printf "\x1b[38;5;173m%s\x1b[0m = " name;
      (match v with
      | Some t -> print_terms [t];
      | None -> ()
      )
   | t :: ts -> 
      print_results [t];
      printf "\n";
      print_results ts