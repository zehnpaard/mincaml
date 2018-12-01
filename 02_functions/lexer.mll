{
open Parser
open Type
}

let space =  [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
  | space+ { token lexbuf }
  | "(*" { comment lexbuf; token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | digit+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
  | '-' { MINUS }
  | "let" { LET }
  | "in" { IN }
  | "rec" { REC }
  | ',' { COMMA }
  | '_' { IDENT(Id.gentmp Type.Unit) }
  | ';' { SEMICOLON }
  | eof { EOF }
  | lower (digit|lower|upper|'_')* { IDENT (Lexing.lexeme lexbuf) }
  | _ { failwith (Printf.sprintf "unknown token %s near characters %d-%d"
                    (Lexing.lexeme lexbuf)
                    (Lexing.lexeme_start lexbuf)
  (Lexing.lexeme_end lexbuf)) }
and comment = parsr
  | "*)" { () }
  | "(*" { comment lexbuf; comment lexbuf }
  | eof { Format.eprintf "warning: unterminated comment@." }
  | _ { comment lexbuf }
