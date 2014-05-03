open Is_valid
open Definition
open Own_util
open Robber
open Build
open Util

(* Player color plays a knight. *)
let incr_knights color plist = 
	List.map (fun (pcol, phand, (pk, px, py)) ->
		if (pcol = color) then (pcol, phand, (pk+1,px,py)) else
		(pcol, phand, (pk, px, py))) plist

(* If player color has card, remove one copy of it
	and return the new player list, otherwise None. *)
let get_card plist color card : player list option= 
	let (_, (_, deck), _) = get_player color plist in 
	let (deck', found) =  match deck with 
	| Hidden _ -> failwith "get_card: You don't have access to this info!"
	| Reveal deck -> 
		(List.fold_right (fun v (ls, found) ->
			if (found || v <> card) then (v::ls, found) else
			(ls, true))  deck ([], false)) in
	if (not found) then None else 
		let plist = if (card = Knight) then
			incr_knights color plist else plist in
		Some (List.map (fun (col, (res, cards), troph) ->
			if (col = color) then (col, (res, Reveal deck'), troph)
			else (col, (res, cards), troph)) plist)

let play_card turn : turn = 
	{active=turn.active;dicerolled=turn.dicerolled;
		cardplayed=true; cardsbought=turn.cardsbought; tradesmade=turn.tradesmade;
		pendingtrade=turn.pendingtrade}

(* Plays a knight, returning the new turn, robber, 
	and the modified player list. *)
let play_knight plist color turn rob_move robber structs= 
	match (get_card plist color Knight) with
		| None -> None
		| Some plist' -> Some (
			let turn' =  play_card turn in
			let res = do_robber_move color rob_move structs robber plist' in
			match res with
			| None -> (robber, turn, plist')
			| Some (robber', plist', _) -> (robber', turn', plist')
			)

let play_road_build plist color turn r1 r2_o structs = 
	match (get_card plist color RoadBuilding) with
		| None -> None
		| Some plist' -> 
			if (can_build_roads_free r1 r2_o color structs) then
				let structs' = build_road structs r1 color in
				let structs' = (match r2_o with
					| None -> structs'
					| Some r2 -> build_road structs' r2 color) in
				let turn' = play_card turn in
				Some (structs', turn', plist')
			else None

let play_year_of_plenty plist color turn r1 r2_o = 
	match (get_card plist color YearOfPlenty) with
		| None -> None
		| Some plist' ->
			let plist' = List.map (fun (cur_col, (cur_inv, cur_cards), cur_trophs) ->
			if (cur_col <> color) then (cur_col, (cur_inv, cur_cards), cur_trophs) else
			let cost' = single_resource_cost r1 in 
			let cost' = match r2_o with
				| None -> cost'
				| Some r2 -> sum_of_two_costs (single_resource_cost r2) cost' in
			let cur_inv' = sum_of_two_costs cur_inv cost' in
			(cur_col, (cur_inv', cur_cards), cur_trophs)) plist' in
			let turn' = play_card turn in
			Some (plist', turn')

let play_monopoly plist color turn res = 
	match (get_card plist color Monopoly) with
		| None -> None
		| Some plist' ->
			let num_found = ref 0 in
			let plist' = List.map (fun (cur_col, (cur_inv, cur_cards), cur_trophs) ->
				if (cur_col = color) then (cur_col, (cur_inv, cur_cards), cur_trophs) else
				let (cur_inv', dfound) = deplete_res cur_inv res in
				num_found := (!num_found + dfound);
				(cur_col, (cur_inv', cur_cards), cur_trophs)
				)	 plist' in 
			let plist' = List.map (fun (cur_col, (cur_inv, cur_cards), cur_trophs) ->
				if (cur_col <> color) then (cur_col, (cur_inv, cur_cards), cur_trophs) else
				let cur_inv' = give_res cur_inv res !num_found in
				(cur_col, (cur_inv', cur_cards), cur_trophs)) plist' in
			let turn' = play_card turn in
			Some (plist', turn')

let doPlay ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) card = 
	if turn.cardplayed then None else
 	match card with
    | PlayKnight robber_move -> 
      begin match (play_knight plist color turn robber_move robber structs) with
        | None -> None
        | Some (robber', turn', plist') ->
          Some (None, ((map, structs, deck, Knight::discard, robber'), plist', turn', (color,req)))
      end
    | PlayRoadBuilding ((_, r1), r2') ->
      begin match (play_road_build plist color turn r1 (road_to_line_option r2') structs) with
    	| None -> None
    	| Some (structs', turn', plist') ->
    	  Some (None, ((map, structs', deck, RoadBuilding::discard, robber), plist', turn', (color,req)))
      end 
    | PlayYearOfPlenty (r1, r2_o) ->
      begin match (play_year_of_plenty plist color turn r1 r2_o) with
    	| None -> None
    	| Some (plist', turn') ->
    		Some (None, ((map, structs, deck, YearOfPlenty::discard, robber), plist', turn', (color,req)))
      end
    | PlayMonopoly res ->
      begin match (play_monopoly plist color turn res) with
      	| None -> None
      	| Some (plist', turn') ->
      		Some (None, ((map, structs, deck, Monopoly::discard, robber), plist', turn', (color,req)))
      end