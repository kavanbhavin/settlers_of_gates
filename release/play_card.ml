open Is_valid
open Definition
open Own_util
open Robber

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
			let (robber', plist') = do_robber_move color rob_move structs robber plist Do_Nothing in
			(robber', turn', plist')
			)

let play_road_build plist color turn = 
	match (get_card plist color RoadBuilding) with
		| None -> None
		| Some plist' -> failwith "hi"
			

let doPlay ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) card = 
	if turn.cardplayed then None else
 	match card with
    | PlayKnight robber_move -> 
      begin match (play_knight plist color turn robber_move robber structs) with
        | None -> None
        | Some (robber', turn', plist') ->
          Some (None, ((map, structs, deck, discard, robber'), plist', turn', (color,req)))
      end
    | PlayRoadBuilding (r1, r2) ->
      failwith "hi"
    | _ -> failwith "unimplemented"