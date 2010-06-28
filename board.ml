(* http://paste.pocoo.org/show/177553/ *)

exception Fill_InvalidSize
exception OutOfBounds
exception InMove_Rollback

type position = int * int

type accessor = 
    Linear of position * (int * int) (* Linear line, with the last tuple being the direction vector *)
  | Horizontal of position
  | Vertical of position

let (++) (a,b) (c,d) = (a+c) , (b+d)

let copy_matrix m (sx,sy) def =
  let r = Array.make_matrix sx sy def in
    for i = 0 to sx-1 do
      r.(i) <- Array.copy m.(i)
    done;
    r

let cut = function 
  | [] -> ([],[])
  | x::xs -> (x, xs)

let rec take n l = match (n,l) with
  | (_, []) -> []
  | (0, _) -> []
  | (n, x::xs) -> x::(take (n-1) xs)

let rec repeat e = function
  | 0 -> []
  | n -> e::(repeat e (n-1))

let rec repeat_num n = function
  | c when c = n -> []
  | c -> c::(repeat_num n (c+1))

let rec lookup k = function
  | [] -> failwith "Not found"
  | (f, v)::xs -> if (f k) = true then v else lookup k xs

let const a (_:'a) = a

let int_al () = [( (=) 1, ('1', const ' '));
		 ( (=) 2, ('2', const '*'));( (=) 3, ('3', const ' '));
		 ( (=) 4, ('4', const '*'));( (=) 5, ('5', const ' '));
		 ( (=) 6, ('6', const '*'));( (=) 7, ('7', const ' '));
		 ( (=) 8, ('8', const '*'));( (=) 9, ('9', const ' '))] (*Default representation for ints - adds a star next to even numbers*)

;;

let test = [| [| 1; 4; 7|];
	      [| 2; 5; 8|];
	      [| 3; 6; 9|];
	      [| 1; 2; 3|] |]

class ['a] board (sx, sy) empty = 
object (self)
  val mutable board = Array.make_matrix sx sy (empty : 'a)
  val mutable in_move = false
  val mutable move_content = []
  val mutable history = []
    
  initializer in_move <- false; move_content <- []; history <- []
  
  method start_move = in_move <- true
  method end_move = in_move <- false; history <- (move_content::history); move_content <- []

  method rollback = 
    let rollback' xs = List.map (fun (pos, e) -> self#raw_set pos e) xs
    and (hd, tl) = cut history
    in 
      if not in_move then begin ignore (rollback' hd); history <- tl;  end
      else raise InMove_Rollback
    
  method private log move = if in_move then move_content <- (move::move_content) else ()

  method get_board = board 
  method copy = 
    let n = new board (sx,sy) empty
    in n#fill board; n 
	
  method fill nb = 
    let p  = Array.length nb = sx
    and p' = snd (Array.fold_left (fun (s, valid) e -> if (Array.length e = s && Array.length e = sy) && (valid = true)
			     then (s, true) else (s, false)) 
		    (Array.length nb.(0),true) nb)
    in if p && p' then board <- (copy_matrix nb (sx,sy) empty) else (raise Fill_InvalidSize)
	
  (* The association list is of type (key, (char_value, pred)). The predicates return a char added after the char_value (used to differenciate colors, for ex *)
  method print al = (* Takes an association list to know how to represent various types *)
    let separator = repeat "-----+" sx in
      print_string "\n   +"; List.iter print_string separator; print_string "\n";
      for j = sy-1 downto 0 do
	Printf.printf " %d |" (j);
	for i = 0 to sx-1 do
	  match self#raw_get (i,j) with
	    | e when e = empty -> print_string "     |"
	    | e -> let (char_val, pred) = lookup e (al ())
	      in Printf.printf " %c%c  |" (pred e) char_val 
	done;
	print_string "\n   +"; List.iter print_string separator; print_string "\n";
      done;
      let repeat_nums = repeat_num sx 0
      and repeat_space = repeat "     " sx
      in print_string "\n  "; List.iter2 (fun x y -> print_string x; print_int y) repeat_space repeat_nums; print_string "\n"
	  
  method set_point p e = self#raw_set p e
  method delete p = self#raw_set p empty
  method move s e = self#raw_set e (self#raw_get s); self#raw_set s empty
  method get_point p = self#raw_get p
  method get_interval p s e u = List.fold_left (fun acc e -> if (acc && e) then true else false) true (List.map p (self#interval s e u))
  method get = function 
      Horizontal p -> self#get (Linear (p, (0,1)))
    | Vertical p   -> self#get (Linear (p, (1,0)))
    | Linear (p, c) -> self#linear p c

  (* Private methods *)
	
  method private interval s e u = 
    let rec interval' = function
      | c when c <> e && self#in_bounds c -> (self#raw_get c)::(interval' (c++u))
      | c when self#in_bounds c -> []
      | _ -> raise OutOfBounds
    in interval' (s++u)
  method private linear (x,y) (a,b) = 
    let reduced = match (a,b) with
      | (_, 0) -> (0, y)
      | (0, _) -> (x, 0)
      | (_, _) -> let m = min x y in (x-m, y-m)
    in let rec linear' = function 
      | (x', y') when self#in_bounds (x',y') -> (self#raw_get (x', y'))::(linear' (x'+a, y'+b))
      | _ -> []
    in linear' reduced
  method in_bounds (x,y) = (x >= 0) && (x < sx) && (y >= 0) && (y < sy)
  method private raw_get (x,y) = board.(x).(y);
  method private raw_set (x,y) e = self#log ((x,y), self#raw_get (x,y)); board.(x).(y) <- e
end;;
