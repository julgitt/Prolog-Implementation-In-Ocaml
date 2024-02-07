type reason =
   | EofInComment
   | InvalidNumber   of string
   | InvalidChar     of char
   | UnexpectedToken of string

exception Parse_error      of (Lexing.position * reason)
exception Runtime_error    of string

exception Cannot_open_file of
   { fname  : string
   ; msg    : string
   }


let format_position (position : Lexing.position) =
   let line   = string_of_int position.pos_lnum in
   let column = string_of_int (position.pos_cnum - position.pos_bol) in
   (line, column)


let string_of_parse_error (line, column) reason =
   let open Printf in
   match reason with
   | EofInComment ->
      sprintf "End of file in comment at %s:%s" line column
   | InvalidNumber x ->
      sprintf "Invalid number '%s' at %s:%s" x line column
   | InvalidChar x ->
      sprintf "Invalid char '%s' at %s:%s" (String.make 1 x) line column
   | UnexpectedToken x ->
      sprintf "Unexpected token '%s' at %s:%s" x line column

    
let string_of_error error =
   match error with
   | Parse_error (position, reason)  ->
      string_of_parse_error (format_position position) reason
   | Cannot_open_file { fname; msg } ->
      Printf.sprintf "Cannot open file \"%s\": %s" fname msg
   | Runtime_error msg -> 
      Printf.sprintf "Runtime error: %s" msg
   | _ -> "Unknown error occurred"