open Util
open Definition 
open Constant
open Own_util
open Discard

let resources_of_corners (resource: resource) (acc: player list) (point: point) (intersections, roads) : player list = 
	match List.nth intersections point with 
	| Some (color, settlement) -> 
	let to_be_added = (map_cost (fun num -> num*(settlement_num_resources settlement)) (single_resource_cost resource))
	in List.map (fun (pcolor, (inventory, cards), trophies) -> 
		if pcolor= color then 
let new_inventory = sum_of_two_costs inventory to_be_added
in (pcolor, (new_inventory, cards), trophies)
else (pcolor, (inventory, cards), trophies)) acc
	| None -> acc


let generate_resources (roll: roll) ((map: map), (structures: structures), (deck: deck), (discard: discard), (robber: robber)) (plist: player list) = 
let (hexes, _) = map in 
let hexes_with_corners = List.mapi (fun index (terrain, roll') -> (index, terrain, roll', piece_corners index)) hexes
in let relevant_hexes = List.filter (fun (index, _, roll', _)-> roll=roll' && (robber != index)) hexes_with_corners
in List.fold_left 
(fun acc (_, terrain, roll, corners) -> 
match resource_of_terrain terrain with 
| Some resource -> List.fold_left (fun acc' point -> resources_of_corners resource acc' point structures) acc corners
| None -> acc
 ) plist relevant_hexes


let roll_dice plist board color turn : next * (player list) * turn = 
	let x = random_roll () in  
 let turn' = { active = turn.active; 
 dicerolled = Some x; 
 cardplayed= turn.cardplayed; 
 cardsbought= turn.cardsbought; 
 tradesmade= turn.tradesmade; 
 pendingtrade= turn.pendingtrade;}
in (* (Printf.printf "dice rolled %d \n" x); *)
	if x = cROBBER_ROLL
	then match get_next_discard_player (next_turn turn.active) turn.active plist with 
			| Some (color_to_send_request) -> ((color_to_send_request, DiscardRequest), plist, turn')
			| None -> ((color, RobberRequest), plist, turn')
 	else ((color, ActionRequest), (generate_resources x board plist), turn') 