open Definition
open Util
open Is_valid

let get_colors points (intersections, roads) = 
	List.fold_left (fun acc point-> match (List.nth point intersections) with 
		| Some (color, settlement)-> color::acc
		| None -> acc) [] points

let do_robber_move (piece, color) (map, structs, deck, discard, robber) plist =
(*	
if (is_valid_piece piece) 
then let robber' = piece 
in let available_colors = get_colors (piece_corners piece) structs 
in if List.mem color available_colors
then failwith "doesn't do anything"
else failwith "color not available next to piece"
else failwith "trying to place robber on invalid piece"
*)
failwith "blah"