let h s =  (Sdlvideo.surface_info s).Sdlvideo.h;;
let w s =  (Sdlvideo.surface_info s).Sdlvideo.w;;
let cote = 67;;
let bx, by = 0, 535;;

type p = 
  | B 
  | W
  | E
;;
module Compare_move =
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
    let l = ref [] in
      for i = 0 to 7 do
	for j = 0 to 7 do
	  let x,y = bx+i*cote + cote/2, by-(j*cote+cote/2) in
	  let c = color_piece s x y in
	    (* on "tourne" la board si on est les noirs *)
	  let coord = if color_bot = White then (i,j) else (7-i, 7-j) in
	    l := (coord, c)::!l;
	done
      done;
      !l

  (* Compare deux scans d'image et trouve les différences (fonction aux) *)
  let rec find_diff back = function
    | [] -> []
    | e::l ->
	if not $ List.mem e back then 
	  e::find_diff back l
	else
	  find_diff back l

  (* à partir du chemin d'une image et de la couleur du bot, *)
  (* génère la liste du materiel sur la board                *)
  let get_list bot_color img = 
    let s = Sdlvideo.load_BMP img in
      binarise s;
      get_map s bot_color
   (* à partir de deux listes générer par get_list, retourne le coup joué *)
  let get_move  bl l = 
    match find_diff bl l with
	(* soit c'est un déplacement normal *)
      | [(c0, E); (c1, turn)] -> (c0,c1)
	  (* sinon si c'est un roque ou une prise en passant *)
      | l  ->
	  (* les prises en passant et roques demandent un traitement particulier car ils engendrent 4 modification du plateau *)
	  let get_x = fun x -> fst (fst x) in
	  let get_y =  fun x -> snd (fst x) in
	    (* si c'est un roque *)
	    if List.for_all (fun e -> let y = get_y e in (y = 0 || y = 7)) l then
	      let xk0, yk0 = fst $ List.find (fun e -> let x = get_x e in x = 4) l in
		(* on cherche le mouvement du roi, qui marque le roque *)
	      let xk1, yk1 = fst $ List.find (fun e -> let x = get_x e in (x =(xk0+2)) || (x=(xk0-2))) l  in
		((xk0, yk0), (xk1, yk1))
	    else 
	      (* si c'est une prise en passant *)
	      let x0, y0 = fst $ List.find (fun e -> let y = get_y e in y = 4 || y = 3) l in
	      let x1,y1 = fst $ List.find (fun e -> let y = get_y e in y = 5 || y = 2) l in
		((x0, y0), (x1, y1))
end
  
