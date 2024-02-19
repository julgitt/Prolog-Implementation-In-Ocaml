open Ast
open Printf

let main_color s =
   sprintf "\x1b[38;5;199m%s\x1b[0m" s

let sub_color s =
   sprintf "\x1b[38;5;212m%s\x1b[0m" s

let sub2_color s =
   sprintf "\x1b[38;5;239m%s\x1b[0m" s
   
let _print_var s =
   printf  "%s" (sub_color s)

let _print_symbol s =
   printf "%s" (main_color s)

let print_error error =
   printf "\x1b[1;31m%s    ඞ sus ඞ\x1b[0m\n" (Errors.string_of_error error)

let print_true () = 
   print_endline ("\x1b[38;5;40mtrue.   ( • ᴗ - ) ✧\n\x1b[0m ")

let print_false () = 
   print_endline ("\x1b[38;5;196mfalse.   ( • ᴖ • ｡)\n\x1b[0m")

let rec view t =
   match t with
   | Var (_, x) -> ( 
      match x with
      | None -> t
      | Some t -> view t
      )
   | _ -> t

let rec print_terms q =
   match q with
   | [] -> ()
   | [t] -> (
      match view t with
      | Var(name, _) -> _print_var name
      | Sym(name, []) -> _print_symbol name
      | Sym(name, xs) -> 
         _print_symbol name;
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
      printf "%s = " (sub_color name);
      (match v with
      | Some t -> print_terms [t];
      | None -> ()
      )
   | t :: ts -> 
      print_results [t];
      printf "\n";
      print_results ts