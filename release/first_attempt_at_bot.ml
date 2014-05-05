open Definition
open Registry 
open Constant
open Util
open Own_util
open Build
open Best_road

let name = "first_attempt_at_bot"

module Bot = functor (S : Soul) -> struct
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = ()
  let find_non_desert hex_list intersections =
  let p2 = ref 0 in 
  let hexes_with_indexes = List.mapi (fun index (a,b) -> (index, a, b) ) hex_list
in let (index, _, _) = List.find 
(fun (index, terrain, _) -> 
	if terrain != Desert 
	then List.exists
	(fun point-> 
		match List.nth intersections point with 
			| Some (_) -> false
			| None -> ( p2 := point); true ) (piece_corners index)
else false) hexes_with_indexes
in (index, !p2)

	let color_with_most_resources plist own_color: color= 
	let (color1, (inventory1, _),_) = match List.hd plist with 
	| (color', _, _) -> if color'=own_color then List.nth plist 2 else List.hd plist 
in let acc = (color1, (sum_cost inventory1))
in let result = 
	List.fold_left 
	(fun (color, maxresources) (color', (inventory, _), _) -> 
	if color' != own_color 
	then let their_resources = sum_cost inventory in
	 	if their_resources >= maxresources 
		then (color', their_resources)
		else (color, maxresources)
	else (color, maxresources)
) acc plist
in fst result
	
	let find_piece_to_rob (color: color) (intersections: intersection list) : piece = 
	let intersections_with_indexes = List.mapi (fun index a-> (index, a)) intersections
in let (index_to_rob, _) = List.find (fun (index, intersection) -> 
	match intersection with 
		| Some (color', _) -> if color'= color then true else false
		| None -> false) intersections_with_indexes 
in index_to_rob

let list_of_my_towns structures my_color = 
	let intersections_with_indexes = List.mapi (fun index a -> (index, a)) (fst structures)
in List.filter (fun (_, a) -> 
	match a with
	| Some (color, _) -> (color = my_color)
	| None -> false ) intersections_with_indexes

let have_enough_resources_city (b,w,o,g,l) = 
	let (bn, wn, on, gn, ln) = cCOST_CITY in 
	b >= bn && w >= wn && o >= on && g >= gn && l >= ln

let have_enough_resources_card (b,w,o,g,l) = 
	let (bn, wn, on, gn, ln) = cCOST_CARD in 
	b >= bn && w >= wn && o >= on && g >= gn && l >= ln
let have_enough_resources_road (b,w,o,g,l) = 
	let (bn, wn, on, gn, ln) = cCOST_ROAD in 
	b >= bn && w >= wn && o >= on && g >= gn && l >= ln
(*let find_road rlist intersections my_color = let my_rlist = List.filter (fun (c, _ ) -> if c = my_color then true else false) rlist
		in match my_rlist with 
			| (c, (p1, p2))::tl -> let other = best_road_from_point p2 intersections rlist
		in (Printf.printf "returning road %d to %d\n" p2 other; (p2, other))
			| _ -> failwith "noroads"*)
let rec play_valid_card cards plist c structures= 
	match cards with 
		| hd::tl -> 
		begin match hd with 
			| Knight -> let color_to_rob = color_with_most_resources plist c in  Action (PlayCard (PlayKnight (
      ((find_piece_to_rob color_to_rob (fst structures)), Some (color_to_rob)))))
			| VictoryPoint -> play_valid_card tl plist c structures 
			| RoadBuilding -> play_valid_card tl plist c structures 
			| YearOfPlenty -> play_valid_card tl plist c structures 
			| Monopoly -> Action (PlayCard (PlayMonopoly (Brick))) (* ideally would find max of resources*) 
		end
		| [] -> Action (EndTurn)
let can_build_town_specific ((intersections, rlist): structures) (col: color) (plist: player list) : (point * intersection) list = 
	let intersections_with_indexes = List.mapi (fun i a -> ( i, a)) intersections
in List.filter (fun (i, _) -> 
	can_build_town (intersections, rlist) i col plist) intersections_with_indexes

  (* Invalid moves are overridden in game *)
  let handle_request (((map, structures, deck, discard, robber), plist, turn, next) : state) : move =
    let (c, r) = next in
    match r with
      | InitialRequest -> InitialMove(find_non_desert (fst map) (fst structures))
      | RobberRequest -> 
      let color_to_rob = color_with_most_resources plist c in 
      RobberMove ((find_piece_to_rob color_to_rob (fst structures)), Some (color_to_rob))

      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeResponse(false)
      | ActionRequest -> 
      let my_inventory = get_res c plist in (*if have_enough_resources_road my_inventory
      then Action( BuyBuild(BuildRoad ((turn.active , (find_road (snd structures) (fst structures) turn.active))))) 
  else *)if have_enough_resources_city my_inventory
      then match pick_random (list_of_my_towns structures c) with
      			| Some ((index, _)) -> Action (BuyBuild (BuildCity index))
      			| None -> failwith "no towns?!"
      		else match pick_random (can_build_town_specific structures c plist) with 
      		| Some ((index, _)) -> Action (BuyBuild (BuildTown (index)))
      		| None -> (Printf.printf "not enough for town or city" ); 
      		if have_enough_resources_card my_inventory
      		then Action (BuyBuild (BuildCard)) 
      	else let my_cards = 
      	(let (_, (_, cards), _ ) = (List.find (fun (color, _, _) ->color = c) plist) 
      in reveal cards)
      in if List.length my_cards = 0 then Action (EndTurn) else play_valid_card my_cards plist c structures

end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
