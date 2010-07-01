open Chess
open Opens
open Aux

(* Ces valeurs dépendent de votre navigateur, et de votre résolution*)

(* Coordonnées absolue du centre de la première case A1 sur l'écran *)
let ax0,ay0 = 619, 749;;
(* Coordonnées du debut du tableau*)
let t0x,t0y = ax0+537, ay0-311;;
(* Coordonnées de la fin du tableau *)
let t1x, t1y = ax0+758, ay0 + 28;;
(* position en x de du centre du tableau des coups *)
let tbl_limit = ax0 + 654;;
(* Longueur qui sépare le centre des cases A1 et A2 (en pixel) *)
let length = 70;;

let print_coord (x,y) = 
  Printf.printf "(%d, %d) " x y
;;
let sleep s = ignore (Unix.select [] [] [] s);;

let get_bot_color () = 
  let r = exec [|"./color.sh"; string_of_int ax0; string_of_int ay0|] in
    if r = ["#FFFFFFFFFFFF  white"] then White
    else Black
;;

let drag_drop (x0,y0) (x1,y1) = 
  ignore $ Sys.command (Printf.sprintf "./cursor/dragdrop %d %d %d %d" x0 y0 x1 y1)
;;


let make_web_move dep = 
  (* make the web move in stockfish*)
  ()

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


(* Ce module a pour vocation de reparer les coups sur chesscube et de les identifier *)
(* il se compose de deux fonctions utiles :  wait_another_play color_bot qui attend que l'adversaire ai joué *)
(* et de get_move () qui lit le coup de l'adversaire sur chesscube *)

module Readmove = 
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


  (* Compare s2 dans s1 en partant de (x,y) et renvoit un pourcentage de similitude *)
  let compare_surface s1 s2 x y = 
    let ws2 = (Sdlvideo.surface_info s2).Sdlvideo.w in
    let hs2 = (Sdlvideo.surface_info s2).Sdlvideo.h in
    let c = ref 0 in
      for i = 0 to ws2-1 do
	for j = 0 to hs2-1 do
	  try
	    let c1 = Sdlvideo.get_pixel s1 (i+x) (j+y) in
	    let c2 = Sdlvideo.get_pixel s2 i j in
	      if c1 = c2 then c := !c+1
	  with _ -> ()
	done
      done;
      (float_of_int !c) /. (float_of_int (ws2*hs2))

  (* Cherche s2 dans s1, et renvoit un pourcentage de similitude (le mieux possible) ainsi que la position éventuelle de s2 dans s1 *)
  let is_in s1 s2 = 
    let ws1 = (Sdlvideo.surface_info s1).Sdlvideo.w in
    let hs1 = (Sdlvideo.surface_info s1).Sdlvideo.h in
    let ws2 = (Sdlvideo.surface_info s2).Sdlvideo.w in
    let hs2 = (Sdlvideo.surface_info s2).Sdlvideo.h in
    let r = ref 0. in
    let coord = ref (0,0) in
      assert(ws1>=ws2 && hs1 >= hs2);
      for x = 0 to ws1-ws2 do
	for y = 0 to hs1 - hs2 do
	  let nr = compare_surface s1 s2 x y in
	    if nr > !r then
	      begin
		r :=  nr;
		coord := (x,y)
	      end
	done
      done;
      (!r, !coord)
  (* Cette fonction "gomme" se s2 de s1 en partant de (x,y), en remplaceant s2 par un rectangle de couleur `823250l` *)
  let gomme s1 s2 x y = 
    let ws2 = (Sdlvideo.surface_info s2).Sdlvideo.w in
    let hs2 = (Sdlvideo.surface_info s2).Sdlvideo.h in
    let r = Sdlvideo.rect x y ws2 hs2 in
      Sdlvideo.fill_rect ~rect:r s1 823250l;
      s1

  (* cette fonction cherche des images dans surface_img, et les gomme.*)
  (* values est une liste de couple string * path *)
  (* pour chaque élement de cette liste, classifie va regarder si l'image du path est dans surface_img *)
  (* si oui elle renvoit string et continue (bien lire le code pour comprendre précisiement ce qu'elle fait) *)
  let classifie surface_img values = 
    let folder = "vignettes/" in
    let rec find surface = function
      | [] -> Sdlvideo.save_BMP surface "imgr.bmp"; ""
      | (s, img)::l ->
	  let s2 = Sdlvideo.load_BMP(folder^img) in
	  let r, (x,y) = is_in surface s2 in
	    if r >= 0.85 then 
	      begin
		s^(find (gomme surface s2 x y) ((s,img)::l))
	      end
	    else
	      find surface l

    in
      find surface_img values
 
  (* Ici on a un groupe de fonctions pour lire les petites images sur chesscube et ainsi faire de l'ocr *)
  let find_piece s = 
    let v = [("R", "R.bmp"); ("K", "K.bmp"); ("Q", "Q.bmp"); ("B", "B.bmp"); ("N", "N.bmp")] in
      classifie s v

  let castling s = 
    let v = [("O-O-O", "ooo.bmp"); ("O-O", "oo.bmp")] in
      classifie s v
  let find_letter s = 
    let v = [("a", "a.bmp"); ("b", "b.bmp"); ("c", "c.bmp"); ("d", "d.bmp"); ("e", "e.bmp"); ("f", "f.bmp");("g", "g.bmp");("h", "h.bmp");] in
      classifie s v
  let find_digit s = 
    let v = [("1", "1.bmp"); ("2", "2.bmp"); ("3", "3.bmp"); ("4", "4.bmp"); ("5", "5.bmp"); ("6", "6.bmp");("7", "7.bmp");("8", "8.bmp");] in
      classifie s v
  let prise s = classifie s [("x", "x.bmp")]
  let echec s = classifie s [("+", "echec.bmp")]
  let prom s = classifie s [("=", "p.bmp")]


  (* Cette fonction lit un coup sur chesscube et le renvoit dans la notation png*)
  let get_move () = 
    let resize() = 
      let x,y = find_last_case () in
	ignore $ Sys.command "convert img.bmp img.png";
	Sys.remove "img.bmp";
	let c = Printf.sprintf "convert img.png -crop %dx%d+%d+%d img.png" 50 20 (x-t0x+3) (y-t0y+1) in
	  ignore $ Sys.command c;
	  ignore $ Sys.command "convert img.png img.bmp";
	  Sys.remove "img.png"
    in
      resize();
      let s = Sdlvideo.load_BMP "img.bmp" in
      let ms c = String.make 1 c in
      let result = 
	match castling s with
	    (* Si c'est un roque : *)
	  | "O-O" -> "O-O"
	  | "O-O-O" -> "O-O-O"
	  | _ ->
	      let n = find_digit s in
	      let is_prise = prise s = "x" in
	      let is_echec = echec s = "+" in
	      let is_prom = prom s = "=" in
	      let l = find_letter s in
		print_endline l;
		let r = 
		  match String.length l with
		    | 2 ->
			(ms (l.[0]))^(if is_prise then "x" else "")^(ms (l.[1]))^n
		    | 1 ->
			(if is_prise then "x" else "")^(ms (l.[0]))^n
		    | _ -> failwith "pas de lettre dans les coordonnées du coup"
		in
		let r' = 
		  if not is_prom then (find_piece s)^r else r^"="^(find_piece s)
		in
		  r'^(if is_echec then "+" else "")
      in
	result ^ (echec s)
end

let rec read_move game chess_o = 
  let s = read_line(print_string "Move : ") in
    try
      ignore $ chess_o#pgn_to_move game s;
      s
    with _ -> read_move game chess_o
;;

(* Par un coup dans de la forme "Algebraic chess notation" *)
(* Necessite une classe chess comprenant l'état du jeu     *)
(* Usage :    parse_move objet_chess string_move           *)
(*       Exemple :                                         *)
(* let g = new chess in g#init; parse_move g "e2e4"        *)

let parse_move g s = 
  let x0 = int_of_letter (s.[0]) in
  let y0 = int_of_char (s.[1]) in
  let x1 = int_of_letter (s.[2]) in
  let y1 = int_of_char (s.[3]) in
  let prom = try piece_type_of_char (s.[4]) with _ -> Queen in
    let r, mvt = g#check_move (x0,y0) (x1,y1) prom true in
      (r, mvt)
;;

let main () = 
  (* On récupère la couleur du robot *)
  let color_bot = get_bot_color () in
    if color_bot = White then print_endline "bot blanc" else print_endline "bot noir";

    (* On initialise le simulateur *)
  let game = new chess in
    game#init;
  let chess_o = new opening in
    (* Ainsi qu'un compteur de coups *)
  let n = ref (if color_bot = White then 1 else 0)  in

    while true do
      (* on regarde a qui c'est  de jouer *)
      let move = 
	if !n mod 2 = 0 
	then (Readmove.wait_another_play color_bot; Readmove.get_move ())
	else read_move game chess_o
      in
	print_endline move;
	let dep = 
	  ( try
	      chess_o#pgn_to_move game move 
	    with _ -> 
	      chess_o#pgn_to_move game (read_move game chess_o)
	  )
	in

	  game#move_piece dep;
	  game#print;

	 if !n mod 2 = 1 then make_bot_move color_bot dep;
	 n := !n+1
    done

;;
let _ = 
  main ()

;;

