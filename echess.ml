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


let read_move_web () = 
  (* Make the ocr here *)
  read_line(print_string "Web move : ")
;;
let read_move_motor() = 
  (* Interface with stockfish here *)
  let r = read_line(print_string "Motor move : ") in  (* Unix.sleep 2;*) r
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
;;
let delete_tmp_img() = 
  Sys.remove "img.bmp";;

(* Regarde si l'adversaire a joué *)
let is_another_play color_bot = 
  let x,y = find_last_case() in
    delete_tmp_img();
    if x = 0 then false
    else if color_bot = White then
      x >= tbl_limit-20
    else
      x <= tbl_limit-20
;;
let wait_another_play color_bot = 
  while not (is_another_play color_bot) do
    sleep 0.5
  done
;;
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
;;

let is_in s1 s2 = 
   let ws1 = (Sdlvideo.surface_info s1).Sdlvideo.w in
   let hs1 = (Sdlvideo.surface_info s1).Sdlvideo.h in
   let ws2 = (Sdlvideo.surface_info s2).Sdlvideo.w in
   let hs2 = (Sdlvideo.surface_info s2).Sdlvideo.h in
   let r = ref 0. in
     assert(ws1>=ws2 && hs1 >= hs2);
     for x = 0 to ws1-ws2 do
       for y = 0 to hs1 - hs2 do
	 let nr = compare_surface s1 s2 x y in
	   r := max (!r) nr;
       done
     done;
     !r
;;
(* A partir de l'image bmp, regarde si la pièce déplacée est un fou, un roi, etc. ou un pion *)
let classifie img = 
  let surface_img = Sdlvideo.load_BMP img in
  let folder = "vignettes/" in
  let v = [("R", "r.bmp"); ("K", "k.bmp"); ("Q", "q.bmp"); ("B", "b.bmp"); ("N", "n.bmp")] in
  let rec find = function
    | [] -> ""
    | (s, img)::l ->
	let surface = Sdlvideo.load_BMP(folder^img) in
	  if is_in surface_img surface >= 0.95 then s
	  else
	    find l
  in
    find v
;;

let get_move () = 
  let x,y = find_last_case () in
    ignore $ Sys.command "convert img.bmp img.png";
    Sys.remove "img.bmp";
    let c = Printf.sprintf "convert img.png -crop %dx%d+%d+%d img.png" 50 20 (x-t0x+5) (y-t0y+1) in
      ignore $ Sys.command c;
      let s =  (List.hd $ exec [|"gocr"; "img.png"|]) in
	ignore $ Sys.command "convert img.png img.bmp";
	classifie "img.bmp" ^ s
;;



let main () = 
  let color_bot = get_bot_color () in
    if color_bot = White then print_endline "bot blanc" else print_endline "bot noir";
  let game = new chess in
    game#init;
  let chess_o = new opening in
  let n = ref (if color_bot = White then 1 else 0)  in
    while true do
      let move = if !n mod 2 = 0 then read_move_web() else read_move_motor() in

       let dep = chess_o#pgn_to_move game move in
	 game#move_piece dep;

	 if !n mod 2 = 1 then make_bot_move color_bot dep
	 else (wait_another_play color_bot; print_endline $ get_move ());

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

;;

