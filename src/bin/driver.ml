(* driver *)

module L = Lexing

(* get and show all tokens from lexical buffer *)
let scan lexbuf =
  (* an auxiliary local recursive function to loop *)
  let rec go () =
    (* extract the next token *)
    let tok = Lexer.token lexbuf in
    (* show the extracted token *)
    Format.printf
      "%a %s\n"
      Location.pp_location (Location.curr_loc lexbuf)
      (Lexer.show_token tok);
    (* check if it is end of file *)
    match tok with
    | Parser.EOF -> ()
    | _ -> go ()
  in
  (* calls the auxiliary function to start looping *)
  go ()

let main () =
  (* turns on recording of exception backtraces for debugging *)
  Printexc.record_backtrace true;

  (* parse command line arguments *)
  Cmdline.parse_cmdline ();

  (* create lexical buffer *)
  let lexbuf = L.from_channel (Cmdline.get_input_channel ()) in
  Lexer.set_filename lexbuf (Cmdline.get_input_file_name ());

  try
    (* should the tokens be printed? *)
    if Cmdline.get_show_tokens () then
      scan lexbuf

    (* compile *)
    else
      let ast = Parser.program Lexer.token lexbuf in
      if Cmdline.get_show_ast () then (
        print_endline "Abstract syntax tree:";
        print_endline "============================================================";
        print_endline (Absyn.show_lexp ast);
        let tree = Tree.map Absyntotree.node_txt (Absyntotree.tree_of_lexp ast) in
        print_endline (Tree.string_of_tree tree);
        print_newline ();
        let boxtree = Tree.box_of_tree tree in
        print_endline (Box.string_of_box boxtree);
        let dotchannel = open_out "ast.dot" in
        output_string dotchannel (Tree.dot_of_tree "AST" tree);
       );
      print_endline "Semantic analysis:";
      print_endline "============================================================";
      let t = Semantic.type_check ast in
      print_endline (Types.show_ty t)
  with
  | Error.Error (loc, msg) ->
     Format.printf "%a error: %s\n" Location.pp_location loc msg;
     exit 1
  | Parser.Error ->
     Format.printf "%a error: syntax\n" Location.pp_position lexbuf.L.lex_curr_p;
     exit 2

(* call the main function *)
let () = main ()
