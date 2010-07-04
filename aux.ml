open Chess

exception ProcessFailed

let ($) f a = f a;;
let piece_chars = [(King, 'K'); (Queen, 'Q'); (Rook, 'R'); (Bishop, 'B'); (Knight, 'N'); (Pawn, 'P')]
let rec rassoc x lst =
    match lst with
    | [] -> raise Not_found
    | (a, b) :: t -> if x = b then a else rassoc x t

let piece_type_of_char c = rassoc (Char.uppercase c) piece_chars
let char_of_piece_type pt = List.assoc pt piece_chars

let int_of_char c = int_of_char c - int_of_char '0' - 1
let char_of_digit d = (string_of_int d).[0];;
let int_of_letter x = 
    let x = Char.lowercase x in
    if 'a' <= x && x <= 'h' 
        then int_of_char x - int_of_char 'a'
        else raise Not_found
let letter_of_int x = "abcdefgh".[x]

let is_digit c = '0' <= c && c <= '9'

let castling_line game = 
    if game#turn = White then 0 else 7
let promotion_line game = 
    if game#turn = White then 6 else 1
let last_line game = 
    if game#turn = White then 7 else 0




let exec t = 
  let read pid descr =
    (* We'll be reading at most 160 characters at a time, I don't know if there's
     * a better way to do it: more, less, adptive. No idea but this should be good
     * enough *)
    let s = String.make 160 '_' in
      (* This function reads everything available from a descriptor and returns
       * when there's nothing more available (yet) *)
    let read_once descr =
      let rec read_once_rc accu =
	(* check if there's something to read: a timeout of 0.02 to minimize
	 * latency, shouldn't cost anything *)
	match Unix.select [ descr ] [] [] 0.02 with
          | [ _ ],  _, _ -> begin
              match Unix.read descr s 0 160 with
		  (* got as much as we asked for, there's probably more: try again*)
		| 160 as l -> read_once_rc ((String.sub s 0 l) :: accu)
		    (* got less than asked, return *)
		| l -> (String.sub s 0 l) :: accu
            end
              (* ok, we got a timeout: return *)
          | _ -> accu
      in
	read_once_rc []
    in
    let rec read_rc pid descr accu =
      (* As long as the process is alive, we have to wait for it to send more data
       * even if nothing is available yet, but when it dies, it becomes unable to
       * write more and we can read everything available in one big pass *)
      match Unix.waitpid [ Unix.WNOHANG ] pid with
	  (* still alive: read and start again, '0' because no child had its state
	   * changed (we're using WNOHANG) *)
	| 0, _ -> read_rc pid descr ((read_once descr) :: accu)
	    (* we know there won't be anything added now: we eat the remaining
	     * characters and return right after that *)
	| _, Unix.WEXITED 0 -> (read_once descr) :: accu
	    (* all other cases, we'll say the process failed and raise an exception *)
	| _, _ -> raise ProcessFailed
    in
    let l = read_rc pid descr [] in
    let ll = List.fold_left (fun a b -> List.rev_append b a) [] l in
    let s = String.concat "" ll in
      (* Split on \r\n newlines on windows and \n newlines elsewhere *)
      match Sys.os_type with
	| "Win32" -> Str.split (Str.regexp "\r\n") s
	| _ (* Uniw | Cygwin *) -> Str.split (Str.regexp "\n") s
  in
  let x, y = Unix.pipe () in 
  let pid = Unix.create_process t.(0) t Unix.stdin y Unix.stderr in 
    read pid x
;;
