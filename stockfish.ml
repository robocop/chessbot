(* permet l'utilisation de stockfish                                    *)
(* utilisation : init(), puis send_move "e2e4" pour jouer e2e4          *)
(* puis : let c = search_move 10 pour trouver un coup a jouer en 10 sec *)
(* puis ne pas oublier de jouer ce coup dans stockfish send_move c      *)
module Stockfish = 
struct
  let open_s ()= Unix.open_process "stockfish"
    (* On ouvre stockfish *)
  let stdin, stdout = open_s()

  let read_stdin cond stdin = 
    let rec read_stdin' stdin r = 
      let s = input_line stdin in
	print_endline s; 
	if Str.string_match r s 0 then s
	else
	  read_stdin' stdin r
    in
      read_stdin' stdin (Str.regexp cond)
  let send stdout cmd = output_string stdout (cmd^"\n"); flush stdout
  let init() = 
    send stdout "uci"; ignore (read_stdin "uciok" stdin);
    send stdout "isready"; ignore (read_stdin "readyok" stdin);
    send stdout "ucinewgame"
  let send_move move =
    send stdout ("position moves "^move)
      
  let search_move time = 
    send stdout "go infinite";
    Unix.sleep time;
    send stdout "stop";
    let s = read_stdin "bestmove .* ponder .*" stdin in
      (Array.of_list (Str.split (Str.regexp " ") s)).(1)

  let print_board () = 
    send stdout "d";
    ignore (read_stdin "Key is: .*" stdin)
end
