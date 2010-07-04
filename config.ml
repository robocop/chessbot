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
