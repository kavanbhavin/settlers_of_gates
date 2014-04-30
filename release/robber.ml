open Definition
open Util
open Is_valid
open Own_util

type robber_failure = Do_Nothing | Move_Robber

let color_exists piece (intersections, _) color = 
	let to_check = piece_corners piece 
	in List.fold_left (fun acc element -> 
		match List.nth intersections element with 
		| Some (color', _) -> (color'=color) || acc
		| None -> false || acc ) false to_check

let rec min_valid_robber active_player structs robber plist = 
	let new_location = 
		if robber = 0 then 1
		else 0
	in do_robber_move active_player (new_location, None) structs robber plist 
(*can cut down significantly on arguments, many are unused*)
and do_robber_move (active_player: color) ((piece: piece), color) (structs: structures) (robber: robber) (plist: player list) (failure: robber_failure) : (robber * player list) =
	let index_to_remove = ref (-1) in 
	if (is_valid_piece piece) 
	then try let robber' = piece in 
			begin match color with  
				| None -> (robber', plist)
				| Some color_to_rob -> 
					if not(color_exists piece structs color_to_rob) 
					then (piece, plist)
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
				in (robber', (List.map (fun (color_of_player, (inventory, cards), trophies)-> 
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
					else (color_of_player, (inventory, cards), trophies) ) plist'))
			end 
		with _ -> (piece, plist)
	else begin match failure with 
				| Do_Nothing -> (robber, plist)
				| Move_Robber -> min_valid_robber active_player structs robber plist Do_Nothing
		end 