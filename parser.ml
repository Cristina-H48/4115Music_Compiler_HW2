open Tokens
exception ParseError of string
(* type ast_node =
  | ProgramNode of ast_node list

  | HeaderNode of ast_node list
  | AttributeNode of string * ast_node * ast_node (* e.g., (Attribute name, operator, value) *)
  | TrackNode of ast_node list
  | PlayNode of ast_node * ast_node * ast_node * ast_node list * ast_node (* play, (, M, ), ; *)


  | NoteNode of ast_node * ast_node (* note and duration *)
  | RepeatNode of ast_node * ast_node * ast_node list * ast_node (* repeat, (, M, ) *)


  | KeywordNode of string
  | OperatorNode of string
  | IdentifierNode of string
  | NumberNode of int
  | StringLiteralNode of string

  | LParenNode
  | RParenNode
  | SemicolonNode
  | CommaNode *)



(*CFG of our languaage, LL(1), No left recurison*)
type expr= (*E->A=V ....TBC?*)
  | Assign of token * token * token
type header=(* H->E;H |epsilon *)
  | Empty
  | Set of expr * token * header
type melody=  (*melody=MUSICNOTE*DURATION*)
  | Melody of token * token
type music_sequence=(*M->melody M'|repeat(M)M'*)
  | Note of melody * music_sequence_suc
  | Repeat of token * token * music_sequence * token * music_sequence_suc
and music_sequence_suc=(*M'->epsilon|,M*)
  | Empty
  | Next of token * music_sequence
type track=  (*T->play (M); *)
  | Play of token * token * music_sequence * token * token
type program=  (* S->HT end*)
  | Program of header * track * token




let next_token tokens=(*pointer that return the next token and the rest of the token sequence after moving the pointer*)
  match tokens with
  | []->raise (ParseError "Unexpected end of input")
  | token :: rest->(token, rest)


let rec parse_program tokens=(*START SYMBOL S*)
  let (header, tokens)=parse_header tokens in
  let (track, tokens)=parse_track tokens in
  let (end_token, tokens)=next_token tokens in
  match end_token with
  | KEYWORD "end" ->
      (Program (header, track, end_token), tokens)
  | _ ->
      raise (ParseError "Syntax error: Expected 'end' keyword")

and parse_header tokens=(*H*)
  match tokens with
  | (KEYWORD "composer"):: _
  | (KEYWORD "instrument"):: _
  | (KEYWORD "bpm"):: _
  | (KEYWORD "title"):: _ ->
      let (expr, tokens)=parse_expr tokens in
      let (semicolon, tokens)=next_token tokens in
      if semicolon=SEMICOLON then
        let (header_rest, tokens)=parse_header tokens in
        (Set (expr, semicolon, header_rest),tokens)
      else
        raise (ParseError "Syntax error: Expected ';' after attribute assignment")
  | _ ->(Empty, tokens)

and parse_expr tokens=(*parse E*)
  let (attribute, tokens)=next_token tokens in
  match attribute with
  | KEYWORD _ ->
      let (operator, tokens)=next_token tokens in
      if operator= OPERATOR "=" then
        let (value, tokens)=next_token tokens in
        (Assign (attribute,operator,value),tokens)
      else
        raise (ParseError "Syntax error: Expected '=' operator")
  | _ ->
      raise (ParseError "Syntax error: Expected attribute keywords - instrument, bpm, title, composer ")

and parse_track tokens= (*T*)
  let (play_keyword, tokens)=next_token tokens in 
  match play_keyword with
  | KEYWORD "play" ->
      let (lpar, tokens)=next_token tokens in
      if lpar= LPAREN then
        let (music_seq,tokens)=parse_music_sequence tokens in
        let (rparen,tokens)=next_token tokens in
        if rparen=RPAREN then
          let (semicolon, tokens)=next_token tokens in 
          if semicolon=SEMICOLON then
            (Play(play_keyword, lpar, music_seq, rparen, semicolon),tokens)
          else
            raise (ParseError "Syntax error: Expected ';' after track")
        else
          raise (ParseError "Syntax error: Missing ')' after music sequence")
      else
        raise (ParseError "Syntax error: Missing '(' after 'play'")
  | _ ->
      raise (ParseError "Syntax error: Expected 'play' keyword to start declaring track chunk")  

and parse_music_sequence tokens=(*M*)
  match tokens with
  | (MUSICNOTE _) :: _ ->(*first production rule for M*)
      let (melody,tokens)= parse_melody tokens in
      let (m_suc,tokens)=parse_music_sequence_suc tokens in
      (Note (melody, m_suc), tokens)
  | (KEYWORD "repeat") :: _ ->(*second production rule for M*)
      let (repeat,tokens)=next_token tokens in
      let (lpar,tokens)=next_token tokens in 
      if lpar=LPAREN then
        let (inner_music_seq, tokens)=parse_music_sequence tokens in
        let (rpar, tokens) =next_token tokens in 
        if rpar=RPAREN then
          let (m_suc, tokens)=parse_music_sequence_suc tokens in
          (Repeat (repeat, lpar, inner_music_seq, rpar, m_suc),tokens)
        else
          raise (ParseError "Syntax error: Expected ')' after repeat sequence")
      else
        raise (ParseError "Syntax error: Missing '(' after 'repeat'")

  | _ ->
      raise (ParseError "Syntax error: Expected a music note or function like 'repeat'")

and parse_music_sequence_suc tokens=(*M'*)
  match tokens with
  | COMMA :: _->
      let (comma, tokens)=next_token tokens in
      let (music_seq, tokens)=parse_music_sequence tokens in
      (Next (comma, music_seq),tokens)
  | _ ->(Empty, tokens)

and parse_melody tokens=(*Single melody consists of pitch and duration*)
  let (note_token,tokens)=next_token tokens in 
  match note_token with
  | MUSICNOTE _->
      let (duration_token,tokens)=next_token tokens in 
      match duration_token with
      | DURATION _ ->
          (Melody (note_token, duration_token), tokens)
      | _ ->
          raise (ParseError "Syntax error: Expected a duration after the music note")
  | _ ->
      raise (ParseError "Syntax error: Expected a music note")

