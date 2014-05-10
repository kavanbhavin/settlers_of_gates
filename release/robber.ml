open Definition
open Util
open Is_valid
open Own_util
open Print
open Constant

let color_has_resource color plist = 
	List.fold_left (fun acc (color', (inventory, cards), trophies) -> 
	if color'=color then (sum_cost inventory) > 0 || acc 
	else acc) false plist

let any_color_exists_and_has_resource piece (intersections, _) own_color plist = 
	let corners = piece_corners piece
in List.fold_left (fun acc element -> 
match List.nth intersections element with 
| Some (color', _) -> if (own_color = color')
then acc
else ((color_has_resource color' plist) || acc)
| None -> acc ) false corners


let color_exists piece (intersections, _) color = 
	let to_check = piece_corners piece 
	in List.fold_left (fun acc element -> 
		match List.nth intersections element with 
		| Some (color', _) -> (color'=color) || acc
		| None -> acc ) false to_check

let rec get_new_piece current_piece = 
	let random_piece = Random.int cNUM_PIECES
in if random_piece = current_piece then get_new_piece current_piece else random_piece

	
let do_robber_move (active_player: color) ((piece: piece), color) (structs: structures) (robber: robber) (plist: player list) : (robber * player list * move) option =
	let index_to_remove = ref (-1) in 
	if (is_valid_piece piece) && piece != robber 
	then try let robber' = piece in 
			begin match color with  
				| None -> if any_color_exists_and_has_resource piece structs active_player plist
							then None
						else Some (robber', plist, RobberMove (robber', None))
				| Some color_to_rob -> 
					if not(color_exists piece structs color_to_rob) 
					then None
					else if color_to_rob = active_player
					then None
					else let plist' = (List.map (fun (color', (inventory, cards), trophies) ->
					if color'= color_to_rob
					then let resources= list_of_resources inventory in
						 let indices= randomize [0;1;2;3;4] in 
						 let inventory'= (index_to_remove := List.find (fun index -> (List.nth resources index)>0) indices);
								(List.mapi (fun index element-> if index = (!index_to_remove) 
																then element-1 
																else element
 											) 	
								resources) in
							(color', ((resources_of_list inventory'), cards), trophies)
					else (color', (inventory, cards), trophies)) plist)
				in Some (robber', (List.map (fun (color_of_player, (inventory, cards), trophies)-> 
					if color_of_player = active_player 
					then let inventory'= 
						let resources = list_of_resources inventory in 
							resources_of_list 
							(List.mapi (fun index element-> 
								if index= (!index_to_remove) 
								then element+1 
								else element) 
							resources) in 
				(color_of_player, (inventory', cards), trophies) 
					else (color_of_player, (inventory, cards), trophies) ) plist'), RobberMove (robber', color))
			end 
		with _ -> None 
	else None

let min_valid_robber active_player (intersections, roads) robber plist = 
let new_location = get_new_piece robber 
in let corners_of_random_piece = piece_corners new_location
in let colors = List.fold_left (fun acc index -> 
	match List.nth intersections index with 
		| Some (color, _) -> if (color= active_player) then acc else color::acc
		| None -> acc
 ) [] corners_of_random_piece
in let random_color_to_rob = pick_random (List.filter (fun color -> color_has_resource color plist) colors)
in match do_robber_move active_player (new_location, random_color_to_rob) (intersections, roads) robber plist with 
		| Some (x) -> Some (x)
		| None -> 
	None




