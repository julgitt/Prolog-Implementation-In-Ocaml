{
let raise_error (lexbuf : Lexing.lexbuf) reason =
  raise (Errors.Parse_error(Lexing.lexeme_start_p lexbuf, reason))

let sym_map =
  let open MenhirParser in
  [ ",",  COMMA
  ; ".",  DOT
  ; ":-", RULE
  ; "*",  ASTERISK
  ; "-",  MINUS
  ; "+",  PLUS
  ; "/",  SLASH
  ; "is", IS
  ] |> List.to_seq |> Hashtbl.of_seq

let tokenize_sym str =
  match Hashtbl.find_opt sym_map str with
  | None     -> MenhirParser.SYM str
  | Some tok -> tok


let tokenize_num lexbuf str =
  try MenhirParser.NUM (int_of_string str) with
  | Failure _ ->
    raise_error lexbuf (InvalidNumber str)
}

let whitespace = ['\011'-'\r' '\t' ' ']
let sym_char =
  [';' ',' '=' '<' '>' '|' '&' '$' '#' '?'
   '!' '@' ':' '^' '.' '+' '-' '~' '*' '/' ]
let lower     = ['a'-'z']
let var_start = ['A'-'Z' '_']
let digit     = ['0'-'9']
let var_char  = var_start | lower | digit
let filepath = (var_char | '-' | '/' | '.')+ '.' lower+

rule token = parse
    whitespace+              { token lexbuf      }
  | '\n'  { Lexing.new_line lexbuf; token lexbuf }
  | "/*"  { block_comment lexbuf;   token lexbuf }
  | '%'   { skip_line lexbuf;       token lexbuf }
  | '('                      { MenhirParser.PARENTH_OPN }
  | ')'                      { MenhirParser.PARENTH_CLS }
  | '<'                      { MenhirParser.BR_OPN }
  | '>'                      { MenhirParser.BR_CLS }
  | sym_char+           as x { tokenize_sym x      }
  | var_start var_char* as x { MenhirParser.VAR x  }
  | lower     var_char* as x { tokenize_sym x      }
  | digit     var_char* as x { tokenize_num lexbuf x      }
  | filepath            as x { MenhirParser.FILEPATH x}
  | eof                      { MenhirParser.EOF       }
  | _ as x {
      raise_error lexbuf (InvalidChar x)
    }

and block_comment = parse
    '\n' { Lexing.new_line lexbuf; block_comment lexbuf }
  | "*/" { () }
  | eof  {
      raise_error lexbuf EofInComment
    }
  | [^'\n' '*']+ { block_comment lexbuf }
  | '*'          { block_comment lexbuf }

and skip_line = parse
    '\n'     { Lexing.new_line lexbuf }
  | eof      { () }
  | [^'\n']+ { skip_line lexbuf }
