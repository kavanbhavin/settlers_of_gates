open Util
open Definition 
open Constant
open Own_util

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
	let hexes_with_corners = List.mapi (fun index (terrain, roll') -> (terrain, roll', piece_corners index)) cDEFAULT_HEXES
in let relevant_hexes = List.filter (fun (_, roll', _)-> roll=roll') hexes_with_corners
in List.fold_left 
(fun acc (terrain, roll, corners) -> 
match resource_of_terrain terrain with 
| Some resource -> List.fold_left (fun acc' point -> resources_of_corners resource acc' point structures) acc corners
| None -> acc
 ) plist relevant_hexes
 
let players_who_need_discards plist = 
	List.filter (fun (_, (inventory, _), _) -> (sum_cost inventory) > cMAX_HAND_SIZE ) plist 
		
let doAction (action: action) (plist: player list) (board: board) : player list = 
	match action with 
 			| RollDice -> let x = random_roll () in  
 						if x = cROBBER_ROLL
 						then failwith "do robber shit"
 						else generate_resources x board plist 
            | MaritimeTrade (maritimetrade) -> failwith "maritimetrade"
            | DomesticTrade (trade) -> failwith "trade"
            | BuyBuild (build) -> failwith "BuyBuild"
            | PlayCard (playcard) -> failwith "playcard"
            | EndTurn -> failwith "EndTurn"
