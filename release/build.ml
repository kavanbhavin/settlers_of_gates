open Definition
open Is_valid
open Constant
open Own_util
open Trophies

(* BUILD STUFF *)

let build_settlement (inters, rlist) p1 color settle_type : structures = 
  if not (empty_settlement p1 inters) then 
    failwith "Trying to override settlement." 
  else 
    ((List.mapi (fun i el -> if (i = p1) then 
        Some (color, settle_type) else el) inters), rlist)

(* Upgrades a town to a city. Precondition: 
 	There is a town at loc, which is a valid index. *)
 let upgrade_town (inters, rlist) loc color : structures = 
 	((List.mapi (fun i el -> if (i = loc) then 
        Some (color, City) else el) inters), rlist)

let build_road (inters, rlist) line color plist : structures * (player list) = 
  if (not (empty_road line rlist)) then 
    failwith "Trying to override road"
  else 
  	let structs' = (inters, ((color, line)::rlist)) in
  	let plist' = update_longest_road plist structs' in
  	(structs', plist')

 (* Checks if we can build a road (enough resources and its free / in range. *)
 let can_build_road (inters, rlist) line col plist = 
 	let (b,w,o,g,l) = get_res col plist in
 	let (bn, wn, on, gn, ln) = cCOST_ROAD in 
	let enough_res = b >= bn && w >= wn && o >= on && g >= gn && l >= ln in
	let is_free = empty_road line rlist in 
	let in_range = road_in_range col (inters, rlist) line in
	let not_too_many = (get_num_roads col rlist) < cMAX_ROADS_PER_PLAYER in 
	enough_res && is_free && in_range && not_too_many

(* Goes through the process of building a road, returning the (structs, plist) pair
	where structs is the updated structures and plist is the player list
	with resources updated for the proper player. Precondition: Player can afford road. *)
let build_road_move (inters, rlist) line color plist = 
	let plist' = List.map (fun (col, (res, hand), troph) ->
		if (col = color) then (col, ((shrink_res res cCOST_ROAD), hand), troph)
		else (col, (res, hand), troph)) plist in
	let (structs', plist') = build_road (inters, rlist) line color plist' in
	(structs', plist')

(* Checks if we can build a town (enough resources and its free. *)
let can_build_town (inters, rlist) point col plist = 
	let (b,w,o,g,l) = get_res col plist in
	let (bn, wn, on, gn, ln) = cCOST_TOWN in
	let enough_res = b >= bn && w >= wn && o >= on && g >= gn && l >= ln in
	let is_free = empty_settlement point inters in
	let in_range = settle_in_range col (inters, rlist) point in
	let not_too_many = (get_num_settles col inters Town) < cMAX_TOWNS_PER_PLAYER in
	enough_res && is_free && in_range && not_too_many

(* Goes through the process of building a town, returning the (structs, plist) pair
	where structs is the updated structures and plist is the player list
	with resources updated for the proper player. Precondition: Player can afford town. *)
let build_town_move (inters, rlist) point color plist = 
	let plist' = List.map (fun (col, (res, hand), troph) ->
		if (col = color) then (col, ((shrink_res res cCOST_TOWN), hand), troph)
		else (col, (res, hand), troph)) plist in
	let structs' = build_settlement (inters ,rlist) point color Town in
	(structs', plist')


(* Checks if we can build a city (enough resources and we own a town here). *)
let can_build_city (inters, rlist) point col plist = 
	let (b,w,o,g,l) = get_res col plist in
	let (bn, wn, on, gn, ln) = cCOST_CITY in
	let enough_res = b >= bn && w >= wn && o >= on && g >= gn && l >= ln in
	let settle_info = get_settle point inters in
	let we_own = (match settle_info with
		| None -> false
		| Some (color, typ) -> (typ = Town) && (col = color)) in
	let not_too_many = (get_num_settles col inters City) < cMAX_CITIES_PER_PLAYER in
	enough_res && we_own && not_too_many

(* Goes through the process of building a city, returning the (structs, plist) pair
	where structs is the updated structures and plist is the player list
	with resources updated for the proper player. Precondition: Player owns town / can afford upgrade. *)
let build_city_move (inters, rlist) point color plist = 
	let plist' = List.map (fun (col, (res, hand), troph) ->
		if (col = color) then (col, ((shrink_res res cCOST_CITY), hand), troph)
		else (col, (res, hand), troph)) plist in
	let structs' = upgrade_town (inters ,rlist) point color in
	(structs', plist')


(* Checks if we can build a card (if we have enough resources) *)
let can_build_card col plist deck = 
	let (b,w,o,g,l) = get_res col plist in
	let (bn, wn, on, gn, ln) = cCOST_CARD in
	let enough_res = b >= bn && w >= wn && o >= on && g >= gn && l >= ln in
	let deck = match deck with
		| Hidden _ -> failwith "Deck is hidden!"
		| Reveal x -> x in
	let cards_left = (List.length deck) <> 0 in
	enough_res && cards_left

(* Goes through the process of building a card, returning the new player list
	with a card in exchange for lost resource. Precondition: Player can afford card.*)
let build_card_move color plist turn deck : (turn * player list * deck)= 
	let (card, deck') = match (ran_card deck) with
		| None -> failwith "Trying to build card but deck is empty!"
		| Some x -> x in
	let cardsbought' = Reveal (match turn.cardsbought with
		| Hidden _ -> failwith "Player with hidden hand buying cards!"
		| Reveal l -> card::l) in 
	let turn' = {active=turn.active; dicerolled=turn.dicerolled;cardplayed=turn.cardplayed;
		cardsbought = cardsbought'; tradesmade=turn.tradesmade;pendingtrade=turn.pendingtrade} in
	let plist' = List.map (fun (col, (res, hand), troph) ->
		if (col = color) then (col, ((shrink_res res cCOST_CARD), hand), troph)
		else (col, (res, hand), troph)) plist in
	(turn', plist', 	deck')	

let doBuild ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) build = 
  match build with
    | BuildRoad (col, line) -> 
      if (can_build_road structs line color plist) then
        let (structs', plist') = build_road_move structs line color plist in
        Some ((get_winner plist' structs'), ((map, structs', deck, discard, robber), plist', turn, (color, req)))
      else None
    | BuildTown point ->
      if (can_build_town structs point color plist) then
        let (structs',plist') = build_town_move structs point color plist in
        Some ((get_winner plist' structs'), ((map, structs', deck, discard, robber), plist', turn, (color, req)))
      else None 
    | BuildCity point -> 
      if (can_build_city structs point color plist) then
        let (structs',plist') = build_city_move structs point color plist in
        Some ((get_winner plist' structs'), ((map, structs', deck, discard, robber), plist', turn, (color, req)))
      else None
    | BuildCard -> 
      if (can_build_card color plist deck) then
        let (turn', plist', deck') = build_card_move color plist turn deck in
        Some ((get_winner plist' structs), ((map, structs, deck', discard, robber), plist', turn', (color, req)))
      else None




