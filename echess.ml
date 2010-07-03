open Chess
open Opens
open Aux
open Config
open Get_move
open Stockfish

let print_coord (x,y) = 
  Printf.printf "(%d, %d) " x y
;;
let sleep s = ignore (Unix.select [] [] [] s);;


let drag_drop (x0,y0) (x1,y1) = 
  ignore $ Sys.command (Printf.sprintf "./cursor/dragdrop %d %d %d %d" x0 y0 x1 y1)
;;


let calc (x,y) = (ax0+length*x, ay0-length*y);;

let make_bot_move color = function
   | Castling  ((x0,y0),(x1,y1))
   | Enpassant  ((x0,y0),(x1,y1))
   | Dep ((x0,y0),(x1,y1)) ->
       if color = White then
	 drag_drop (calc (x0,y0)) (calc (x1,y1))
       else
	 drag_drop (calc (7-x0,7-y0)) (calc (7-x1,7-y1))
   | _ -> ()
;;


(* Ce module a pour vocation de reparer quand l'autre joue *)
(* il se compose de deux fonctions utiles :  wait_another_play color_bot qui attend que l'adversaire ai joué *)
(* et de get_move () qui lit le coup de l'adversaire sur chesscube *)

module Wait_move = 
struct
  (* Cette fonction cherche la partie bleu `823250l` dans le tableau des coups à droite. *)
  let find_last_case () = 
    let c = Printf.sprintf "import -window root -crop %dx%d+%d+%d img.bmp" (t1x-t0x) (t1y - t0y) t0x t0y in
      ignore $ Sys.command c;
      let s = Sdlvideo.load_BMP "img.bmp" in
      let w = (Sdlvideo.surface_info s).Sdlvideo.w in
      let h = (Sdlvideo.surface_info s).Sdlvideo.h in
      let r = ref (0,0) in
      let f = ref true in
	for i = 0 to w do
	  for j = 0 to h do
	    if !f && (Sdlvideo.get_pixel s i j) =  823250l then (r := (i+t0x,j+t0y); f := false);
	  done
	done;
	!r
  let delete_tmp_img() = 
    Sys.remove "img.bmp"

  (* Regarde si l'adversaire a joué *)
  let is_another_play color_bot = 
    let x,y = find_last_case() in
      delete_tmp_img();
      if x = 0 then false
      else if color_bot = White then
	x >= tbl_limit-20
      else
	x <= tbl_limit-20

  (* Attend que l'adversaire ai bien joué *)
  let wait_another_play color_bot = 
    Unix.sleep 1;
    while not (is_another_play color_bot) do
      Unix.sleep 1
    done
end
;;
(* Lit un coup à la main dans les notation pgn *)
(* Ex : Nf3 *)
let rec read_move game chess_o = 
  let s = read_line(print_string "Move : ") in
    try
      ignore $ chess_o#pgn_to_move game s;
      s
    with _ -> read_move game chess_o

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
    let board0 = ref (get_list bot_color) in
      print_endline "generation board 0 ok";
    while true do
      (* si c'est au moteur de jouer *)
      if !n mod 2 = 1 then
	begin
	   let move = Stockfish.search_move 5 in
	     print_endline move;
	   let _,t = parse_move game move in
	   let dep = get_option t in
	     (* on joue le coup dans le simulateur *)
	     game#move_piece dep;
	     (* on joue le coup dans stockfish *)
	     Stockfish.send_move (write_move dep);
	     (* on joue le coup dans chesscube  *)
	     make_bot_move bot_color dep;
	     let board1 = get_list bot_color in
	       print_endline "generation de la nouvelle board";
	       board0 := board1;
	end
      else 
	begin
	Wait_move.wait_another_play bot_color;
	  print_endline "le joueur a joue";
	let board1 = get_list bot_color in
	print_endline "generation de la nouvelle board";
	let c0,c1 = Read_move.get_move !board0 board1 in
	  print_coord c0; print_newline (); print_coord c1;
	let _,t = get_move game c0 c1 Queen in
	let dep = get_option t in
	  (* on joue le coup dans le simulateur *)
	  game#move_piece dep;
	  (* on joue le coup dans stockfish *)
	  Stockfish.send_move (write_move dep);
	   board0 := board1;
	end;
      n := !n+1;
      game#print
    done

;;
let _ = 
  main ()

;;

