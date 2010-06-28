exception Invalid_pgn;;
exception Invalid_pgn_format;;
exception No_file

open Chess
open Aux
type result = Win | Loose | Draw | Unknow;;



let remove s i = (String.sub s 0 i)^(String.sub s (i+1) (String.length s - i - 1));;
let is_letter = function
  | 'a'..'h' -> true
  | _ -> false;;


class opening = 
object (self)
  val mutable book = []
  val mutable file = None

  method private parse_pgn = 
    let rec parse c s in_read = match (input_line c, in_read) with
      | ("", false) -> parse c s true
      | ("", true) -> s
      | (_, false) -> parse c s in_read
      | (line, true) -> parse c ((if s = "" then "" else s^" ")^line) true
    in 
    let tuple_of_list = function
      | a::b::[] -> (a, b)
      | _ -> raise Invalid_pgn
    in
    let parse_game s =
      (* We split by the numbers moves*)
      let l = Str.split (Str.regexp "[0-9]+\\.") s in 
      (* We split by the spaces *)
      let l_moves = List.map (Str.split (Str.regexp "[ ]+")) l in
      let l' = List.rev l_moves in 
	(* We check the result *)
      let r, l_moves = List.hd (List.rev (List.hd l')), List.rev (List.tl l') in
      let moves = List.map tuple_of_list l_moves in
	match r with 
	  | "1-0" -> (Win, moves)
	  | "0-1" -> (Loose, moves)
	  | "1/2-1/2" -> (Draw, moves)
	  | "*" -> (Unknow, moves)
	  | _ -> print_string r; raise Invalid_pgn_format
    in
      match file with
	| Some c -> parse_game (parse c "" false)
	| None -> raise No_file
  method private forward = 
     let rec tail acc = function
      | [] -> acc
      | (r, _::l)::list when l <> [] ->
	  tail ((r, l)::acc) list
      | _::list -> tail acc list
     in
       book <- tail [] book
  method fill_book fbook turn  = 
    try 
      let f = open_in fbook in
	file <- Some f;
	let moves = ref [] in
	let result = if turn = White then Win else Loose in
	  try
	    while true do
	      let r, m = self#parse_pgn  in
		if m <> [] && (r = result || r = Draw) then moves := (r, m)::!moves
	    done
	  with End_of_file -> book <- !moves
    with Sys_error _ -> raise No_file

  method select_move game move = 
    let f = if game#turn = White then fst else snd in
      book <- List.filter (fun (r, l) -> self#pgn_to_move game (f(List.hd l)) = move) book;
      if game#turn = Black then self#forward

  method get_move (game:chess) = 
    let rec max_l me list = match me, list with
      | me, [] -> me
      | Some(c, nm), (s, n)::l -> max_l (Some (if n > nm then (s, n) else (c, nm))) l
      | None, (s, n)::l -> max_l (Some (s, n)) l
    in
    let rec max_moves f l = function
      | [] -> max_l None l
      | (r, moves)::tail ->
	  let c = f (List.hd moves) in
	    if List.mem_assoc c l then
	      max_moves f ((c, (List.assoc c l)+1)::(List.remove_assoc c l) ) tail
	    else
	      max_moves f ((c, 1)::l) tail
    in

    let f = if game#turn = White then fst else snd in 
    let r = max_moves f [] book in
    let result = begin match r with
	| Some(m, _) ->
	   book <- List.filter (fun (r, l) -> f (List.hd l) = m) book; 
	    Some(self#pgn_to_move game m)
	| None -> None
    end in
      if game#turn = Black then self#forward;
      result

  method get_b = book

  method pgn_to_move (game:chess) s = 
    (* 
       format : 
       ((x, y), (x', y'), piece, promotion, capture) where 
                              promotion = Some piece | None
                              capture = true | false
    *)
    let parse_move s = 
      let r = ref s in
      let colone, line, capture = ref None, ref None, ref false in
      let x, y = ref 0, ref 0 in
      let piece = ref Pawn in
      let promotion = ref None in
	if List.mem s.[0] ['R'; 'Q'; 'B'; 'K'; 'N'] then
	      begin
		piece := piece_type_of_char (s.[0]);
		r := remove s 0
	      end;

	    if (is_letter !r.[0]) && (is_letter !r.[1] || !r.[1] = 'x') then
	      begin
		colone := Some (int_of_letter (!r).[0]);
		r := remove !r 0;
	      end
	    else if (is_digit !r.[0]) && (is_letter !r.[1] || !r.[1] = 'x') then
	      begin
		line := Some (int_of_char (!r).[0]);
		r := remove !r 0;
	      end
	    else if try (is_letter !r.[0]) && (is_digit !r.[1]) && (is_letter !r.[2] || !r.[2] = 'x') with _ -> false then
	      begin
		colone :=  Some (int_of_letter (!r).[0]);
		line := Some (int_of_char (!r).[1]);
		r := remove (remove !r 0) 0;
	      end;
	    if !r.[0] = 'x' then begin r := remove !r 0; capture := true end;
	    x := int_of_letter !r.[0]; y := int_of_char !r.[1];
	    r := remove (remove !r 0) 0;
	    if try !r.[0] = '=' && List.mem !r.[1] ['R'; 'Q'; 'B'; 'K'; 'N'] with _ -> false then 
	      promotion := Some (piece_type_of_char (!r.[1]));

	    ((!colone, !line), (!x, !y), (!piece, !promotion, !capture))
    in
    let pred e = function
      | None -> true
      | Some c -> c = e
    in
    let select (a, b) (a', b') = 
      pred a' a && pred b' b
    in
    let cover_move = function
      | Dep((a, b), (a', b'))
      | Prom((a, b), (a', b'), _) 
      | Enpassant((a, b), (a', b')) -> 
	 ((a, b), (a', b'))
      | _ -> raise Invalid
    in
    let is_promotion = function
      | Prom(_, _, _) -> true
      | _ -> false
    in
    let edit_promotion p = function
      | Prom(d, a, _) -> Prom(d, a, p)
      | _ -> raise Invalid
    in
    let l = game#get_moves true in
      match s with
      | "O-O"   -> Castling((4, castling_line game), (6, castling_line game)) 
      | "O-O-O" -> Castling((4, castling_line game), (2, castling_line game)) 
      | s ->
	  let ((x, y), (x', y'), (piece, promotion, capture)) = parse_move s in
	  let r = List.find (fun m -> 
			       try 
				 let d, a = cover_move m in 
				 select (x, y) d && a = (x', y') && 
				 get_piece (game#board#get_point d) = piece && 
				 (if promotion <> None then is_promotion m else true)
			      with Invalid -> false
			    ) l
	  in
	    match promotion with
	      | Some p -> edit_promotion p r
	      | None -> r
end
;;
