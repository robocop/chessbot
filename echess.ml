open Chess
open Opens

let print_coord (x,y) = 
  Printf.printf "(%d, %d) " x y
;;


let get_bot_color () = 
  (* Get the color of the bot in the game *)
  White
;;

let read_move_web () = 
  (* Make the ocr here *)
  read_line(print_string "Web move : ")
;;
let read_move_motor() = 
  (* Interface with stockfish here *)
  read_line(print_string "Motor move : ")
;;

let make_web_move dep = 
  (* make the web move in stockfish*)
  ()
let make_bot_move dep = 
  (* make the bot move in the web interface with the mouse *)
  ()
let main () = 
  let color_bot = White in
  let game = new chess in
    game#init;
  let chess_o = new opening in
  let n = ref (if color_bot = White then 1 else 0)  in
    while true do
      let move = if !n mod 2 = 0 then read_move_web() else read_move_motor() in
       let dep = chess_o#pgn_to_move game move in
	 game#move_piece dep;
	 n := !n+1;
	 game#print;
	 match dep with
	   | Castling (a,b) 
	   | Enpassant (a,b) 
	   | Dep (a,b) ->  print_coord a; print_coord b
	   | Prom (a,b,p) -> print_coord a; print_coord b
    done

;;
let _ = 
  main ()
