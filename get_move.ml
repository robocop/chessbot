(* Utilisation générale : 
 Read_move.get_bot_color() : renvoit la couleur du bot
 get_list bot_color : génère un plateau avec la couleur des pions de la board
 Read_move.get_move a0 a1 : à partir du nouveau plateau et de l'ancien plateau, determine le coup joué
*)

open Config
open Chess
open Aux

type p = 
  | B 
  | W
  | E
;;

module Read_move =
struct
  let h s =  (Sdlvideo.surface_info s).Sdlvideo.h
  let w s =  (Sdlvideo.surface_info s).Sdlvideo.w
  let int_white =  16777215l
  let int_black = 0l
    (*  Binarise une surface *)
  let binarise s = 
    let hs,ws = h s, w s in
      for i = 0 to hs do
	for j = 0 to ws do
	  let c = Sdlvideo.get_pixel s i j in
	    if c >= Int32.div int_white 3l then
	      Sdlvideo.put_pixel s i j int_white
	    else
	      Sdlvideo.put_pixel s i j int_black
	done
      done
	(* recupère la couleur d'une piece sur l'image : *)
	(* W : blanche, B : noire, et E : pas de pièce   *)
  let color_piece s x y = 
    let n = ref 0 in
    let f = float_of_int in
      for i = x-20 to x+20 do
	for j = y-20 to y+20 do
	  let c = Sdlvideo.get_pixel s i j in
	    if c = int_white then n := !n+1
	done
      done;
      let r = (f !n) /. 1681. in
	if r <= 0.75 then B
	else if r <= 0.95 then W
	else E
  (* Scan l'image pour récupérer la board *)
  let get_map s color_bot = 
    let arr = Array.make_matrix 8 8 E in
      for i = 0 to 7 do
	for j = 0 to 7 do
	  let x,y = i*length + length/2, l_board-(j*length+length/2) in
	  let c = color_piece s x y in
	    (* on "tourne" la board si on est les noirs *)
	  let ax,ay = if color_bot = White then (i,j) else (7-i, 7-j) in
	    arr.(ay).(ax) <- c;
	done
      done;
      arr

  (* Compare deux scans d'image et trouve les différences (fonction aux) *)
  let diff m1 m2 =
    let d = ref [] in
      for x=0 to 7 do
	  for y=0 to 7 do
	    if m1.(y).(x) <> m2.(y).(x) then d := (x,y)::!d;
	  done
	done;
    !d
;;

   (* à partir de deux boards générer par get_map, retourne le coup joué *)
  let get_move  a0 a1 = 
    match diff a0 a1 with
	(* soit c'est un déplacement normal *)
      | [(xa, ya);b] -> 
	  if a1.(ya).(xa) = E then ((xa,ya),b)
	  else (b, (xa,ya))
	  (* sinon si c'est un roque ou une prise en passant *)
      | l  ->
	  (* les prises en passant et roques demandent un traitement particulier car ils engendrent 4 modification du plateau *)
	  let get_x = fst in
	  let get_y =  snd in
	    (* si c'est un roque *)
	    if List.for_all (fun e -> let y = get_y e in (y = 0 || y = 7)) l then
	      let xk0, yk0 = List.find (fun e -> let x = get_x e in x = 4) l in
		(* on cherche le mouvement du roi, qui marque le roque *)
	      let xk1, yk1 =  List.find (fun e -> let x = get_x e in (x =(xk0+2)) || (x=(xk0-2))) l  in
		((xk0, yk0), (xk1, yk1))
	    else 
	      (* si c'est une prise en passant *)
	      let x0, y0 = List.find (fun e -> let y = get_y e in y = 4 || y = 3) l in
	      let x1,y1 = List.find (fun e -> let y = get_y e in y = 5 || y = 2) l in
		((x0, y0), (x1, y1))

	let make_img_board () = 
	  let c = Printf.sprintf "import -window root -crop %dx%d+%d+%d board.bmp" l_board l_board bx (by-l_board) in
       ignore $ Sys.command c

	(* Retourne la couleur du bot *)
	let get_bot_color () = 
	  let r = exec [|"./color.sh"; string_of_int ax0; string_of_int ay0|] in
	    if r = ["#FFFFFFFFFFFF  white"] then White
	    else Black
end
;;

(* à partir du chemin d'une image et de la couleur du bot, *)
(* génère la liste du materiel sur la board                *)
let get_list bot_color = 
  Read_move.make_img_board();
  let s = Sdlvideo.load_BMP "board.bmp" in
    Read_move.binarise s;
    Read_move.get_map s bot_color
;;
