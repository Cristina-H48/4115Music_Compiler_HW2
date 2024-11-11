open Lexer
open Tokens
open Parser
open Ast_printer

let token_tostring token =
  match token with
  | KEYWORD k->Printf.sprintf "<KEYWORD, \"%s\">" k
  | OPERATOR op->Printf.sprintf "<OPERATOR, \"%s\">" op
  | IDENTIFIER id->Printf.sprintf "<IDENTIFIER, \"%s\">" id
  | NUMBER n->Printf.sprintf "<NUMBER, \"%d\">" n
  | STRING_LITERAL s->Printf.sprintf "<STRING_LITERAL, \"%s\">" s
  | MUSICNOTE note->Printf.sprintf "<MUSICNOTE, \"%s\">" note
  | DURATION duration->Printf.sprintf "<DURATION, \"%s\">" duration
  | LPAREN->"<LPAREN, \"(\">"
  | RPAREN-> "<RPAREN, \")\">"
  | SEMICOLON-> "<SEMICOLON, \";\">"
  | COMMA-> "<COMMA, \",\">"
(*let () =
  if Array.length Sys.argv<2 then
    Printf.printf "Usage: %s <input_file>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    (* Use Arg.read_arg to read the content of the file *)
    let lines=Array.to_list (Arg.read_arg filename) in
    let source=String.concat "\n" lines in  (* Join lines into a single string *)

    try
      let tokens=lex source in
      List.iter (fun token ->
        let token_str =string_of_token token in
        Printf.printf "%s\n" token_str;
      )tokens
    with
    | LexingError msg->
        Printf.printf "LexingError: %s\n" msg*)
let ()=
  if Array.length Sys.argv<2 then
    Printf.printf "Usage: %s <input_file>\n" Sys.argv.(0)
  else
    let filename=Sys.argv.(1) in
    let ic=open_in filename in
    let buffer =Buffer.create 500 in
    (try
       while true do
         let line=input_line ic in
         Buffer.add_string buffer line;
         Buffer.add_char buffer '\n'; 
       done
     with 
     End_of_file->());
    close_in ic;
    let source=Buffer.contents buffer in

    try
      let tokens = lex source in
      let (ast, _) = parse_program tokens in
      print_ast ast 0
    with
    | LexingError msg ->
        Printf.printf "Lexing error: %s\n" msg
    | ParseError msg ->
        Printf.printf "Parsing error: %s\n" msg
