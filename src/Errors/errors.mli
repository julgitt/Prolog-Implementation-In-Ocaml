type reason =
  | EofInComment
  | InvalidNumber   of string
  | InvalidChar     of char
  | UnexpectedToken of string

exception Parse_error      of (Lexing.position * reason)
exception Runtime_error    of string
exception Cannot_open_file of { fname : string; msg : string }

val string_of_error : exn -> string