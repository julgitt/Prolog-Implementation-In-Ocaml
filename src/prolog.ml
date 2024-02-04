open Parser
open Ast
open Interpreter
open BacktrackStateMonad.BSM
open Printer


let program : clause list ref = ref []
let origin_variables : ((variable * term option) list ref) = ref []
let all_solutions : ((variable * term option) list) list ref = ref []

let is_solution_duplicate solution =
  List.exists (fun sol -> sol = solution) !all_solutions


let rec add_origin_variables terms =
  let is_variable_present name =
    List.exists (fun (n, _) -> n = name) !origin_variables
  in
  let add_variable name =
    if not (is_variable_present name) then
      origin_variables := (name, None) :: !origin_variables
  in
  match terms with
  | [] -> ()
  | Var (name, _) :: ts ->
    add_variable name;
    add_origin_variables ts
  | Sym (_, ts) :: ts2 ->
    add_origin_variables ts;
    add_origin_variables ts2
  | Num _ :: ts -> 
    add_origin_variables ts


(*    Query Execution   *)

let rec search_for_more_solutions () =
  let* new_goals = backtrack_goals () in
  let* (_, solutions) = evaluate new_goals in
  match solutions with
  | [] ->
    return (print_false ())
  | _ ->
    if not (is_solution_duplicate solutions) then (
      all_solutions := solutions :: !all_solutions;
      print_results solutions;
      wait_for_user_input ()
    ) else
      search_for_more_solutions ()

and wait_for_user_input () =
  let* input = return (read_line ()) in
  match input with
  | ";" ->
    print_endline "";
    search_for_more_solutions ()
  | "." ->
    return (print_endline "")
  | _ ->
    raise (Errors.Runtime_error "Invalid expression - '.' or ';' expected")


let run_query ts =
  run () (
    let* _ = initialize (!program) (!origin_variables) in
    let* (is_solved, solutions) = evaluate ts in
    all_solutions := solutions :: !all_solutions;
    print_endline "";
    (match solutions with
      | [] -> if is_solved 
          then return (print_true ())
          else return (print_false ())
      | _ ->
        print_results solutions;
        wait_for_user_input () 
    )
  )
  

(*  Console initialization   *)
let print_welcome_message () =
  ["\n\x1b[38;5;199m" ^ "PROLOG" ^ "\x1b[0m\n";
   "Type <filepath> to load a program from a file";
   "or type a query.";
   "Press ctrl+d to exit.\n"]
  |> List.iter print_endline


let init_console () =
  LNoise.history_load ~filename:".commands_history" |> ignore;
  LNoise.history_set ~max_length:100 |> ignore;
  print_welcome_message ()


(*    User input handling   *)
let add_input_to_history input =
  LNoise.history_add input |> ignore;
  LNoise.history_save ~filename:".commands_history" |> ignore


let handle_single_input input =
  try
    add_input_to_history input;
    let query = parse_query_string input in
    match query with
    | Filepath f ->
      program := parse_file f;
      print_clauses !program;
      print_endline ""
    | Query ts ->
      origin_variables := [];
      add_origin_variables ts;
      run_query ts
  with error -> print_error error


let user_input_loop prompt action =
  let rec loop () =
    match LNoise.linenoise prompt with
    | None -> ()
    | Some input ->
      action input;
      loop ()
  in
  loop ()


(*    Main    *)
let main_loop () =
  user_input_loop "?- " handle_single_input


let _ =
  init_console ();
  main_loop ()
