(* The first section of the lexer definition, called the *header*,
   is the part that appears below between { and }.  It is code
   that will simply be copied literally into the generated lexer.ml. *)

{
open Parser

exception Error of string
}

(* The second section of the lexer definition defines *identifiers*
   that will be used later in the definition.  Each identifier is
   a *regular expression*.  We won't go into details on how regular
   expressions work.

   Below, we define regular expressions for
     - whitespace (spaces and tabs),
     - digits (0 through 9)
     - integers (nonempty sequences of digits, optionally preceded by a minus sign)
     - letters (a through z, and A through Z), and
     - identifiers (nonempty sequences of letters).

   FYI, these aren't exactly the same as the OCaml definitions of integers and
   identifiers. *)

let whitespace_char_no_newline = [' ' '\t' '\012' '\r']
let non_zero_digit = ['1'-'9']
let digit = ['0'-'9']
let int = non_zero_digit digit*
let letter = ['a'-'z' 'A'-'Z']
let id = letter ['a'-'z' 'A'-'Z' '0'-'9' '_' '?']*

(* The final section of the lexer definition defines how to parse a character
   stream into a token stream.  Each of the rules below has the form
     | regexp { action }
   If the lexer sees the regular expression [regexp], it produces the token
   specified by the [action].  We won't go into details on how the actions
   work.  *)

rule read =
  parse
  | whitespace_char_no_newline+    { read lexbuf }
  | '\n'     { Lexing.new_line lexbuf; read lexbuf }
  | "(*"     { comment lexbuf; read lexbuf } (* activate "comment" rule *)           
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "+"      { PLUS }
  | "-"      { MINUS }
  | "*"      { TIMES }
  | "/"      { DIVIDED }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "{"      { LBRACE }
  | "}"      { RBRACE }
  | "<"      { LANGLE }
  | ">"      { RANGLE }
  | ";"      { SEMICOLON }
  | ":"      { COLON }
  | ","      { COMMA }
  | "."      { DOT }
  | "abs"    { ABS }
  | "let"    { LET }
  | "="      { EQUALS }
  | "in"     { IN }
  | "proc"   { PROC }
  | "zero?"  { ISZERO }
  | "if"     { IF }
  | "then"   { THEN }
  | "else"   { ELSE }
  | "letrec" { LETREC }
  | "set"    { SET }
  | "begin"  { BEGIN }
  | "end"    { END }
  | "newref" { NEWREF }
  | "deref"  { DEREF }
  | "setref" { SETREF }
  | "debug"  { DEBUG }
  | "fst"   { FST }
  | "snd"   { SND }
  | "pair"   { PAIR }
  | "unpair" { UNPAIR }
  | "untuple" { UNTUPLE }
  | "send"   { SEND }
  | "class"   { CLASS }
  | "super"   { SUPER }
  | "extends" { EXTENDS }
  | "method" { METHOD }
  | "field"   { FIELD }
  | "self"   { SELF }
  | "new"   { NEW }
  | "list"   { LIST }
  | "cons"   { CONS }
  | "hd"   { HD }
  | "tl"   { TL }
  | "empty?"   { EMPTYPRED }
  | "implements"  { IMPLEMENTS }
  | "instanceof?"  { INSTANCEOF }
  | "interface"  { INTERFACE }
  | "cast"  { CAST }
  | "int"    { INTTYPE }
  | "bool"   { BOOLTYPE }
  | "unit"   { UNITTYPE }
  | "->"     { ARROW }
  | "ref"    { REFTYPE }
  | id       { ID (Lexing.lexeme lexbuf) }
  | eof      { EOF }
  | _
      { raise (Error (Printf.sprintf
                        "At offset %d: unexpected character."
                        (Lexing.lexeme_start lexbuf))) }
and
  comment = parse
  | "*)" { () }
  | '\n'     { Lexing.new_line lexbuf; comment lexbuf }
  | eof  { failwith "unterminated comment" }
  | _    { comment lexbuf }  (* skip comments *)



(* And that's the end of the lexer definition. *)
