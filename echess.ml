open Chess
open Opens
open Aux
open Config
open Get_move
open Stockfish

let print_coord (x,y) = 
  Printf.printf "(%d, %d) " x y
;;

let drag_drop (x0,y0) (x1,y1) = 
  ignore $ Sys.command (Printf.sprintf "./cursor/dragdrop %d %d %d %d" x0 y0 x1 y1)
;;

let click (x,y) = 
  let c = Printf.sprintf "xdotool mousemove %d %d; xdotool click 1" x y in
    ignore $ Sys.command c
;;

let calc (x,y) = (ax0+length*x, ay0-length*y);;

let make_bot_move color move = 
  let make_move (x0,y0) (x1,y1) = 
     if color = White then
	 drag_drop (calc (x0,y0)) (calc (x1,y1))
       else
	 drag_drop (calc (7-x0,7-y0)) (calc (7-x1,7-y1))
  in
  let l_half_case = length/2 in
  let get_coord x = function
    | Queen -> x-3*l_half_case
    | Rook -> x-l_half_case
    | Bishop -> x+l_half_case
    | Knight -> x+3*l_half_case
    | _ -> x
  in
    match move with
      | Castling  ((x0,y0),(x1,y1))
      | Enpassant  ((x0,y0),(x1,y1))
      | Dep ((x0,y0),(x1,y1)) ->
          make_move (x0,y0) (x1,y1)
      | Prom((x0,y0), (x1,y1), p) ->
          make_move (x0,y0) (x1,y1);
	  let x, y = calc (x1,y1) in
	  click (get_coord x p, y)
;;


(* Ce module a pour vocation de reparer quand l'autre joue *)
(* il se compose de deux fonctions utiles :  wait_another_play color_bot qui attend que l'adversaire ai joué *)
(* et de get_move () qui lit le coup de l'adversaire sur chesscube *)

module Wait_move = 
struct
  (* Cette fonction cherche la partie bleu `823250l` dans le tableau des coups à droite. *)
  let find_last_case color_bot = 
    let c = Printf.sprintf "import -window root -crop %dx%d+%d+%d img.bmp" 2 (t1y - t0y) (if color_bot = White then tbl_limit+10 else t0x) t0y in
      ignore $ Sys.command c;
      let s = Sdlvideo.load_BMP "img.bmp" in
      let w = (Sdlvideo.surface_info s).Sdlvideo.w in
      let h = (Sdlvideo.surface_info s).Sdlvideo.h in
      let r = ref (0,0) in
      let f = ref true in
	while !f do
	  for i = 0 to w do
	    for j = 0 to h do
	      if !f && (Sdlvideo.get_pixel s i j) =  823250l then (r := (i+t0x,j+t0y); f := false);
	    done
	  done;
	  f := false;
	done;
	!r
;;
  let delete_tmp_img() = 
    Sys.remove "img.bmp"

  (* Regarde si l'adversaire a joué *)
  let is_another_play color_bot = 
    let x,y = find_last_case color_bot in
      (* delete_tmp_img(); *)
      if x = 0 then false
      else
	true

  (* Attend que l'adversaire ai bien joué *)
  let wait_another_play color_bot = 
    sleep 0.2;
    while not (is_another_play color_bot) do
      sleep 0.2
    done
     
end
;;

(* Lit un coup à la main dans les notation pgn *)
(* Ex : Nf3 *)
let rec read_move game chess_o = 
  let s = read_line(print_string "Move : ") in
    try
      chess_o#pgn_to_move game s
    with _ -> read_move game chess_o
;;
(* Obtient un déplacement avec deux coordonnées *)
let get_move g (x0, y0) (x1, y1) prom = 
   g#check_move (x0,y0) (x1,y1) prom true
;;
(* Parse un coup dans de la forme "Algebraic chess notation"  *)
(* Necessite une classe chess comprenant l'état du jeu        *)
(* Usage :    parse_move objet_chess string_move              *)
(*       Exemple :                                            *)
(* let g = new chess in g#init; parse_move g "e2e4"           *)
(* Renvoit un bool* dep option :                              *)
(* soit (true, Some dep) si le coup est valide                *)
(* (false, None) si le coup n'est pas valide                  *)
(* utiliser la fonction `get_option` pour récupérer le coup   *)
let parse_move g s = 
  let x0 = int_of_letter (s.[0]) in
  let y0 = int_of_char (s.[1]) in
  let x1 = int_of_letter (s.[2]) in
  let y1 = int_of_char (s.[3]) in
  let prom = try piece_type_of_char (s.[4]) with _ -> Queen in
    g#check_move (x0,y0) (x1,y1) prom true
;;

(* Fonction récriproque de parse_move : à partir d'un dep, renvoit un string *)
let write_move dep = 
  let write (x0,y0) (x1,y1) = 
    let x0 = String.make 1 (letter_of_int x0) in
       let y0 =  String.make 1 (char_of_digit (y0+1)) in
       let x1 = String.make 1 (letter_of_int x1) in
       let y1 =  String.make 1 (char_of_digit (y1+1)) in
	 x0^y0^x1^y1
  in
  match dep with 
   | Castling  ((x0,y0),(x1,y1))
   | Enpassant  ((x0,y0),(x1,y1))
   | Dep ((x0,y0),(x1,y1)) ->
       write (x0,y0) (x1,y1)
   | Prom((x0,y0),(x1,y1),p) -> 
       let p = String.make 1 (Char.lowercase $ char_of_piece_type p) in
	 write (x0,y0) (x1,y1)^"="^p

;;
let get_color_board game = 
  let r = Array.make_matrix 8 8 E in
  let b = game#board#get_board in
    for i = 0 to 7 do
      for j = 0 to 7 do
	match b.(i).(j) with
	  | Empty -> r.(j).(i) <- E
	  | Piece(_, White) ->  r.(j).(i) <- W
	  | Piece(_, Black) ->  r.(j).(i) <- B
      done
    done;
    r
;;

let main () = 
  (* On récupère la couleur du robot *)
  let bot_color = Read_move.get_bot_color () in
    if bot_color = White then print_endline "bot blanc" else print_endline "bot noir";

    (* On initialise le simulateur *)
  let game = new chess in
    game#init;
  let chess_o = new opening in
    (* On initialise stockfish *)
    Stockfish.init();
    print_endline "init stockfish ok";
    (* Ainsi qu'un compteur de coups *)
  let n = ref (if bot_color = White then 1 else 0)  in
    (* On récupère une position de départ pour l'ocr de coups *)
    let board0 = ref (get_color_board game) in
      print_endline "generation board 0 ok";
    while true do
      (* si c'est au moteur de jouer *)
      if !n mod 2 = 1 then
	begin
	   let move = Stockfish.search_move time in
	     print_endline move;
	     let dep = 
	       try
		 let _,t = parse_move game move in
		 get_option t
	       with _ -> read_move game chess_o 
	     in
	       (* on joue le coup dans le simulateur *)
		 game#move_piece dep;

	       (* on joue le coup dans stockfish *)
	       Stockfish.send_move (write_move dep);
	       (* on joue le coup dans chesscube  *)
	       make_bot_move bot_color dep;
	       
	       let board1 = get_color_board game in
		 print_endline "generation de la nouvelle board";
		 board0 := board1
	end
      else 
	begin
	Wait_move.wait_another_play bot_color;
	  print_endline "le joueur a joue";
	  sleep 0.2;
	let board1 = get_list bot_color in
	print_endline "generation de la nouvelle board";
	  let dep = 
	    try 
	      let c0,c1 = Read_move.get_move !board0 board1 in
		print_coord c0; print_newline (); print_coord c1;
		let _,t = get_move game c0 c1 Queen in
		  get_option t
	    with _ ->  
	      begin
		let v = read_move game chess_o in 
		  print_endline "diff : ";
		  List.iter (fun x -> print_coord x) (Read_move.diff !board0 board1);
		  v
	      end
	  in
	      
	  (* on joue le coup dans le simulateur *)
	  game#move_piece dep;
	  (* on joue le coup dans stockfish *)
	  Stockfish.send_move (write_move dep);
	   board0 := board1;
	end
	  ;
      n := !n+1;
      game#print
    done

;;
let _ = 
  main ()

;;

