module Parse =
struct 
  let parse filename =
      let file = open_in filename in
      let lexbuf = Lexing.from_channel file in
      let lexer () = Lexer.tokenize lexbuf in
      let rec do_it () =
	let t = lexer () in
        print_string t;
        print_string "\n";
        if (String.sub t 0 3) = "EOF" then () else do_it ()
      in
      do_it();
      close_in file
end

let () =
  Parse.parse "/home/henrik/programming/ocaml/compiler/lib/testcases/test1.tig"
