open Chess
open Opens
open Aux

(* les valeurs précédés de §§§ dépendent de votre navigateur, et de votre résolution*)
(* les autres normalement de varie pas (normalement) *)

(* §§§ coordonées du coup inférieur gauche de la board *)
let bx, by = 582, 781;; 
(* §§§ longueur de la board *)
let l_board = 539;;

(* longeur d'un coté d'une  case *)
let length = l_board/8;;
(* position en x de du centre du tableau des coups *)
let tbl_limit = bx + l_board + 150;;
(* Coordonnées du debut du tableau*)
let t0x,t0y = bx + l_board+31, by-l_board+209;;
(* Coordonnées de la fin du tableau *)
let t1x, t1y = bx + l_board+233, by-l_board+528;;

(* coordonnées du centre de la premiere case *)
let ax0, ay0 = bx+length/2, by-length/2;;

(* On ouvre stockfish *)
let stdin, stdout = Unix.open_process "stockfish";;

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
	  let x,y = bx+i*length + length/2, by-(j*length+length/2) in
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
      | [(c0, E); (c1, _)] -> (c0,c1)
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
  let make_img_board () = 
     let c = Printf.sprintf "import -window root -crop %dx%d+%d+%d board.bmp" l_board l_board bx (by-l_board) in
       ignore $ Sys.command c
end
  


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

(* permet l'utilisation de stockfish                                    *)
(* utilisation : init(), puis send_move "e2e4" pour jouer e2e4          *)
(* puis : let c = search_move 10 pour trouver un coup a jouer en 10 sec *)
(* puis ne pas oublier de jouer ce coup dans stockfish send_move c      *)
module Stockfish = 
struct
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


let main () = 
  (* On récupère la couleur du robot *)
  let color_bot = get_bot_color () in
    if color_bot = White then print_endline "bot blanc" else print_endline "bot noir";

    (* On initialise le simulateur *)
  let game = new chess in
    game#init;
  let chess_o = new opening in
    (* On initialise stockfish *)
    Stockfish.init();
    (* Ainsi qu'un compteur de coups *)
  let n = ref (if color_bot = White then 1 else 0)  in
    (* On récupère une position de départ pour l'ocr de coups *)
    Compare_move.make_img_board ();
    let l = ref (Compare_move.get_list color_bot "board.bmp") in
    while true do
      let nl = 
	if !n mod 2 = 0 then
	  begin
	    Compare_move.make_img_board ();
	     Compare_move.get_list color_bot "board.bmp"
	  end
	else [] 
      in
	
     
	let dep = 
	  ( try
	      if  !n mod 2 = 0 then
		begin
		  Readmove.wait_another_play color_bot;
		  let m0,m1 = Compare_move.get_move !l nl in
		    print_coord m0; print_coord m1;
		  let _,t = get_move game m0 m1 Queen in
		    get_option t
		end
	      else
		let move = Stockfish.search_move 5 in
		let _,t = parse_move game move in
		  get_option t
	    with _ -> 
	      chess_o#pgn_to_move game (read_move game chess_o)
	  )
	in
	  (* on joue le coup dans le simulateur *)
	  game#move_piece dep;
	  (* on joue le coup dans stockfish *)
	  Stockfish.send_move (write_move dep);
	  (* on joue le coup dans chesscube (si c'est à nous de jouer) *)
	  if !n mod 2 = 1 then make_bot_move color_bot dep;
	  (* on change l'état de la map pour l'orc *)
	  l := nl;
	  (* on affiche le plateau *)
	  game#print;
	  Stockfish.print_board();

	 n := !n+1
    done

;;
let _ = 
  main ()

;;

