open Util
open Definition
open Constant


  type objective = NA | OTown of int | OCity of int | OCard | ORoad of int * int
  type to_buy = RoadBuy | TownBuy | CityBuy | CardBuy
  type strategy = SPlayerTrade of int * cost * cost | SMaritimeTrade of resource * resource | SBuildCard | NoStrategy
  	| SPlayYoP of resource * resource | SPlayMonopoly of resource | SPlayKnight of robbermove | SPlayRoadBuild of int * int

(* Returns a certain player from a player list.
THIS FUNCTION FAILS IF THE PLAYER ISN'T IN THE LIST. *)
let get_player color plist =
	try List.find (fun (c, _, _) -> c = color) plist with
	Not_found -> failwith "get_player : Player not found!"


let get_res color plist = 
	let (_, (res, _), _) = get_player color plist in res

let enough_res (b,w,o,g,l) (bn, wn, on, gn, ln) : bool =
	b >= bn && w >= wn && o >= on && g >= gn && l >= ln

let can_pay_cost cost color plist = 
    let have = get_res color plist in
	enough_res have cost

let can_afford want color plist = 
	let have = get_res color plist in
	match want with
	| RoadBuy -> enough_res have cCOST_ROAD
	| TownBuy -> enough_res have cCOST_TOWN
	| CityBuy -> enough_res have cCOST_CITY
	| CardBuy -> enough_res have cCOST_CARD

  let subtract_res (x1a,x2a,x3a,x4a,x5a) (x1b,x2b,x3b,x4b,x5b) = 
    (x1a-x1b, x2a-x2b, x3a-x3b,x4a-x4b,x5a-x5b)

   let add_res_vals (x1a,x2a,x3a,x4a,x5a) (x1b,x2b,x3b,x4b,x5b) = 
    (x1a+x1b, x2a+x2b, x3a+x3b,x4a+x4b,x5a+x5b)

(* Expected number of times a number is rolled 
	out of 36 rolls, scaled as necessary. *)
let expected_of_36 num import_factor = 
	let (a,b,c,d,e,f,g) = (1,2,3,4,5,6,0) in
	let (a,b,c,d,e,f,g) = if (import_factor = 1) then (a,b,c,d,e,f,g) else
	if (import_factor = 2) then (1,3,6,10,15,20,0) else 
	if (import_factor = 3) then (1, 5, 12, 22, 35, 51, 0) else (a,b,c,d,e,f,g)
	in match num with
	| 2 | 12 -> a
	| 3 | 11 -> b
	| 4 | 10 -> c
	| 5 | 9 -> d
	| 6 | 8 -> e
	| 7 -> f
	| _ -> g

(* Returns true if there exists a settlement
< 2 road lengths away from point. *)
let settle_one_away point inters : bool = 
let neighbors = adjacent_points point in
List.fold_left (fun acc v ->
  acc || (match (List.nth inters v) with
    | None -> false
    | Some _ -> true)) false neighbors

(* Checks if we own the resource, in which case weight it 1,
    otherwise weight it power. *)
let res_weight res power reslist = 
	if (List.mem res reslist) then 1 else
	power

let num_owned res reslist = 
	List.fold_left (fun acc v ->
		if (v = res) then acc+1 else acc) 0 reslist

(* Weights a settlement for development using the procedure:
	weight wheat as 5, lumber as 4, all other resources as 3.
	Multiply the final weight by the expected number of times
	the tile is landed out of in 36 rolls, and divide it by the
	number of tiles of this weight we already own. *)
let weight_loc settle num board res_owned inters= 	
	let ((hexlist, _), _, _, _, _) = board in 
	match settle with
		| Some _ -> 0
		| None ->
	if (settle_one_away num inters) then 0 else 
	let hexes_gained = adjacent_pieces num in
	fst (List.fold_left (fun (acc, curlist) hex_gained ->
		try (let (terrain,roll) = List.nth hexlist hex_gained in
		let (acc', div_factor, res, import_factor) =
		 let res_owned' = curlist@res_owned in
		 match (resource_of_terrain terrain) with
			| None -> (-1, 1, [], 0)
			| Some Grain -> (30, 1 + (num_owned Grain res_owned') * 4, [Grain], 2)
			| Some Lumber -> (100, 1 + (num_owned Lumber res_owned') * 4, [Lumber], 3)
			| Some Brick -> (90, 1 + (num_owned Brick res_owned') * 4, [Brick], 3)
			| Some Wool -> (30, 1 + (num_owned Wool res_owned') * 4, [Wool], 2)
			| Some Ore -> (10, 1 + (num_owned Ore res_owned') * 4, [Ore], 1) in
		let acc' = acc' * (expected_of_36 roll import_factor) / div_factor in
		(acc + acc', res@curlist)
	) with _ -> (acc, curlist)) (0, []) hexes_gained)

(* Adds a resource to the list of resource tiles we own. *)
let add_res loc reslist board = 
	let ((hexlist, _), _, _, _, _) = board in 
	let hexes_gained = adjacent_pieces loc in
	let to_add = List.fold_left (fun acc v -> try
		let (terrain, _) = List.nth hexlist v in
		match (resource_of_terrain terrain) with
			| None -> acc
			| Some x -> x::acc
	with _ -> acc) [] hexes_gained
	in reslist := to_add@(!reslist)

(* Sort intersections in order of increasing desirability. *)
let sort_locs inters board res_tiles = 
	let weighted_inters = List.mapi (fun n inter ->
      (inter, n, (weight_loc inter n board res_tiles inters))) inters in
    List.sort (fun (_, _, w1) (_,_, w2) -> w2 - w1) weighted_inters

(* STUFF COPIED FROM IS_VALID *)

(* If p1 and p2 are adjacent. *)
let valid_pair (p1, p2) : bool = 
  p1 >= 0 && p2 >= 0 && p1 <= cMAX_POINT_NUM && p2 <= cMAX_POINT_NUM && 
  List.mem p2 (adjacent_points p1)

(* Is this settlement within the necessary bounds? *)
let valid_settle point = 
  point >= 0 && point <= cMAX_POINT_NUM  


let empty_road (p1, p2) roads : bool = 
  if (not (valid_pair (p1, p2))) then false else
  not (List.exists (fun (_, line) ->
    (line = (p1, p2) || line = (p2, p1))) roads)

(* Returns "player col owns a road with one point
    on point." *)
let own_road_with_point point col roads : bool = 
  List.fold_left (fun acc (r_c, (r_s, r_e)) -> 
    acc || ((r_s = point || r_e = point) && r_c = col)) false roads

(* PRECONDITION: p1 is a valid indices. *)
let empty_settlement p1 intersections : bool =
  if (not (valid_settle p1)) then false else
  match (List.nth intersections p1) with
  | Some _ -> false 
  | None -> true

(* If p1 and p2 are adjacent and p1 is an unowned
   village and the road from p1 to p2 is unowned. *)
let free_valid_pair (p1, p2) (intersections, roads) : bool = 
  (valid_pair (p1, p2)) && 
    (empty_road (p1, p2) roads) &&
    (empty_settlement p1 intersections)

let is_valid_piece piece = 
  piece>=0 && piece<= cMAX_PIECE_NUM

  (* Given a settlement, return a valid road leaving it,
   or None if this is impossible. *)
let get_valid_road settle structs color: road option = 
  (* Computes a list of all possible roads leaving this settlement. *)
  let poss_roads = List.map (fun v -> (settle, v)) (adjacent_points settle) in
  (* Returns the first valid (untaken) road in poss_roads, or none
    if every road there is taken. *)
  try Some (color, (List.find (fun (start_loc, end_loc) -> 
     free_valid_pair (start_loc, end_loc) structs) poss_roads)) with
      Not_found -> None

  (* Returns the number of settlements of type typ
    owned by player col. *)
  let get_num_settles col settles typ : int = 
    List.fold_left (fun acc v ->
      match v with
      | None -> acc
      | Some (settle_col, settle_type) ->
        if ((col = settle_col) && (typ = settle_type)) then
          acc+1 else acc) 0 settles

  (* Returns the number of roads owned by player col. *)
  let get_num_roads col roads : int =
    List.fold_left (fun acc (road_col, _) ->
    if (road_col = col) then acc + 1 else acc) 0 roads

 (* Extends the frontier one iteration : adds points one further away not in garbage. *)
 let extend_path (inters, roads) (frontier : (int * int * int list) list) (garbage : (int * int * int list) list) (need_road : bool) 
 	(col : color) : ((int * int * int list) list) * ((int * int * int list) list) = 
 	let garbage = ref (garbage@frontier) in 
 	let frontier' = List.fold_left (fun acc (index, distance, path) ->
 		let adj_points = adjacent_points index in 
 		List.fold_left (fun littlacc (neighbor : int) ->
 			let closer = List.exists (fun (i, _, _) -> i = neighbor) !garbage in
 			let owned = match List.nth inters neighbor with
 				| Some _ -> true
 				| None -> false in
 			let road_ok = if need_road then own_road_with_point neighbor col roads else true in
 			let () = garbage:= ((neighbor, 0, [])::(!garbage)) in
 			if (not closer && not owned && road_ok) then
 				(neighbor, distance+1, path@[neighbor])::littlacc else littlacc) acc adj_points) [] frontier in
 	(frontier', !garbage)

(* Return a list of nearby empty settlements, how far away they are from one of our
   settlements, and a quick path from our settlement to theirs. First_done : 
   do we want to require that we use a road we already built as the first step? *)
let get_near_empty ((inters, roads) : structures) (color : color) (first_done : bool) = 
	let settlesi = List.mapi (fun i s -> (s, i)) inters in
	let owned_settles = List.fold_left (fun acc (s, i) ->
		match s with
			| None -> acc
			| Some (col, _) -> if (color <> col) then acc else 
				(i, 0, [i])::acc) [] settlesi in
	let () = print_endline ("owned settles: "^(string_of_int (List.length owned_settles))) in
	let garbage = List.map (fun (i, _, _) -> (i, 0, [])) owned_settles in
	let (one_away, garbage) = extend_path (inters, roads) owned_settles garbage first_done color in
	let () = print_endline ("one away: "^(string_of_int (List.length one_away))) in
	let (two_away, garbage) = extend_path (inters, roads) one_away garbage false color in
	let two_away = List.filter (fun (i, _, _) -> not (settle_one_away i inters)) two_away in
	let () = print_endline ("two away: "^(string_of_int (List.length two_away))) in
	two_away

(* Given the resources we have and those we need, return a list of the ones we need.
	List will be sorted by how badly we need each one for convenience. *)
let res_needed (b,w,o,g,l) (bn,wn,on,gn,ln) : (resource * int) list = 
	let ls = [] in 
	let ls = if (b < bn) then (Brick, (bn - b))::ls else ls in
	let ls = if (w < wn) then (Wool, (wn - w))::ls else ls in
	let ls = if (o < on) then (Ore, (on - o))::ls else ls in
	let ls = if (g < gn) then (Grain, (gn - g))::ls else ls in
	let ls = if (l < ln) then (Lumber, (ln - l))::ls else ls in
	List.sort (fun (_, a) (_, b) -> b - a) ls


(* Is a road empty, owned by us, or by someone else? *)
type road_owner = NoRoad | Us | Them
let road_status col rlist (p1, p2) : road_owner = 
	List.fold_left (fun acc (rcol, (rp1, rp2)) ->
		if ((rp1 = p1) && (rp2 = p2)) || ((rp1 = p2) && (rp2 = p1)) then 
			if (col = rcol) then Us else Them
		else acc
	) NoRoad rlist

(* Given a trail we are trying to follow (series of roads culminating in building a town)
	return the next objective. *)
let rec trail_to_move (inters, roads) color goal_loc (trail : int list) : objective = 
	match trail with
	| [] -> NA
	(* Case: last leg of journey. Build final road or build town if possible. *)
	| start_loc::[] -> OTown goal_loc
	(* Case: not last leg of journey. Build road if needed, otherwise check next part of journey. *)
	| start_loc::between_loc::n2 -> begin match (road_status color roads (start_loc, between_loc)) with
		| NoRoad ->  let () = print_endline ((string_of_int start_loc)^(string_of_int between_loc)) in ORoad (start_loc, between_loc)
		| Us -> trail_to_move (inters, roads) color goal_loc (between_loc::n2)
		| Them -> NA
	end

(* We want to build a town, What is a good road to choose?
	Parallels town_move so that we next try and build a town
	off of one of these roads.  *)
let get_init_road ((inters, roads) : structures) res_tiles board color loc : int * int =
	(* Update inters so that it only contains the player's current location. *)
	let inters = List.mapi (fun i v -> if (i = loc) then Some (color, Town) else
		match v with
			| None -> None
			| Some (col, t) -> if (col = color) then None else Some (col, t)) inters in
	let weighted_locs = sort_locs inters board res_tiles in
	let nearby_options = get_near_empty (inters, roads) color false in
	let rec choose_move wl : objective = 
		match wl with
		| [] -> NA
		| (_, h, _)::t -> 
			let valid_options = List.filter (fun (i, _, _) -> i = h) nearby_options in
			begin match valid_options with
			  | [] -> choose_move t
			  | (loc, dis, trail)::_ -> let () = print_endline (string_of_int (List.length trail)) in 
			  	let () = List.fold_left (fun acc i -> print_endline (string_of_int i)) () trail in 
			  	trail_to_move (inters, roads) color loc trail
			  end in
	let obj = choose_move weighted_locs in match obj with
		| ORoad (p1, p2) -> (p1, p2)
		| _ -> match (get_valid_road loc (inters, roads) color) with
			| Some (a, b) -> b
			| None -> (0, 0)

 (* Update our goal resources so we know what to strive for. *)
 let get_goal_res obj : cost = 
 	match obj with
 	 | NA -> (0,0,0,0,0)
 	 | OTown _ -> cCOST_TOWN
 	 | OCity _ -> cCOST_CITY
 	 | OCard -> cCOST_CARD
 	 | ORoad _ -> cCOST_ROAD

 (* Given a player, return the probability that we will randomly
 	pick one of the resources we need from them. *)
 let get_prob_pick_card color plist need =
 	let (_, (res, _), _) = get_player color plist in
 	let (b,w,o,g,l) = res in 
 	let (bn, wn, on, gn, ln) = need in 
 	let tot_cards = sum_cost res in
 	let tot_valid = ref 0 in
 	let () = if (bn <> 0) then tot_valid:= (!tot_valid + b) else ()  in
 	let () = if (wn <> 0) then tot_valid:= (!tot_valid + w) else ()  in
 	let () = if (on <> 0) then tot_valid:= (!tot_valid + o) else ()  in
 	let () = if (gn <> 0) then tot_valid:= (!tot_valid + g) else ()  in
 	let () = if (ln <> 0) then tot_valid:= (!tot_valid + l) else ()  in
 	let float_num = float_of_int (!tot_valid) /. (float_of_int tot_cards) in
 	(* Scale this to a comparable int. *)
 	let float_scaled = float_num *. 100. in
 	(int_of_float float_scaled)


 (* Pick a robber move to play. *)
 let get_robber_move hexes inters color robber_loc plist need=
 	let hexesi = List.mapi (fun i hex -> i) hexes in
 	(* Try to find a spot that isn't on our land and lets us steal. *)
 	let goal = List.fold_left (fun acc v ->
 			if (v = robber_loc) then acc else
 			let adjacent_locs = piece_corners v in
 			match (List.fold_left (fun (acc, blocks_us) v ->
 				if blocks_us then ([], true) else
 				match (List.nth inters v) with
 					| Some (c, _) -> if (c <> color) then ((v, c)::acc, false) else ([], true)
 					| None -> (acc, blocks_us)
 			) ([], false) adjacent_locs) with
 				| (_, true) -> acc
 				| (l, false) -> l@acc
 		) [] hexesi in 
 	match goal with
 		| _::_ -> 
 	(* We have found a nonempty list of people we can steal from, now sort them by possibility of getting
 		something that we need if we randomly robbed them. *)
		let goal = List.map (fun (loc, col) -> (loc, color, (get_prob_pick_card color plist need))) goal in
		let goal = List.sort (fun (_,_,a) (_, _, b) -> b-a) goal in 
		begin match goal with
			| (loc, col, _)::_ -> (loc, Some col)
			| _ -> (0, None) (* Shouldn't happen, we already checked for list length 0. *)
		end

 	| [] -> 
 	(* We couldn't find a spot : just pick one that isn't on our land. Forget stealing for now. *)
 	let goal = List.fold_left (fun acc v ->
 			if (v = robber_loc) then acc else
 			let adjacent_locs = piece_corners v in
 			match (List.fold_left (fun (acc, blocks_us) v ->
 				if blocks_us then (None, true) else
 				match (List.nth inters v) with
 					| Some (c, _) -> if (c <> color) then (Some c, false) else (None, true)
 					| None -> (acc, blocks_us)
 			) (None, false) adjacent_locs) with
 				| (_, false) -> Some v
 				| (_, true) -> acc
 		) None hexesi in 
 	match goal with
 		| Some v -> (v, None)
 		| None -> (0, None) (* Default move : should never happen. *)

 (* Given a cost, remove negative values. *)
 let normalize (b,w,o,g,l) : cost = 
 	((max b 0), (max w 0), (max o 0), 
 	 (max g 0), (max l 0))
 let cost_to_list (b,w,o,g,l) = [b;w;o;g;l]
 let list_to_cost ls =  match ls with | [b;w;o;g;l] -> (b,w,o,g,l) | _ -> (0,0,0,0,0)

 (* Assume we have to discard, and choose the cards we want
 	to discard. *)
 let get_discard_res curr goal = 
 	let num_to_discard = (sum_cost curr) / 2 in
 	let leftovers = subtract_res curr goal in 
 	let leftovers = normalize leftovers in
 	let num_leftovers = (sum_cost leftovers) in
 	let rec choose_from_leftovers ls largest_el num_removed =
 		let num_removed = ref num_removed in  
 		let ls' = List.map (fun x -> if (x = largest_el && !num_removed < num_to_discard)
 			then let () = (num_removed:= (!num_removed + 1)) in x-1 else x) ls in
 		if (!num_removed < num_to_discard) then choose_from_leftovers ls' (largest_el - 1) !num_removed
		else list_to_cost ls' in 
	(* If we can afford to make the discard with just cards not needed for our goal, do it. *)
	if (num_leftovers >= num_to_discard) then 
		let () = print_endline ("need discard: "^(string_of_int num_to_discard)) in
		let sorted_list = List.sort (fun a b -> b - a) (cost_to_list leftovers) in match sorted_list with
			| h::_ -> let new_hand = choose_from_leftovers (cost_to_list leftovers) h 0 in 
				let (b,w,o,g,l) = new_hand in
				let () = print_endline ("temp: "^(string_of_int b)^(string_of_int w)^(string_of_int o)^(string_of_int g)^(string_of_int l)) in
				subtract_res leftovers new_hand
			| [] -> (0, 0, 0, 0, 0) (* We have enough in leftovers to discard but it's empty? Should never happen. *)
	(* If we must discard cards we do need, then discard all the leftovers plus those we do need.
		Actually, we'll use the same method to choose from those we do need as we did to choose from the leftovers. *)
	else 
		let those_needed = subtract_res curr leftovers in
		let those_needed_sort = List.sort (fun a b -> b - a) (cost_to_list those_needed) in match those_needed_sort with
			| h::_ -> let needed_discard = subtract_res those_needed (choose_from_leftovers (cost_to_list those_needed) h (sum_cost leftovers)) in 
				add_res_vals leftovers needed_discard
			| _ -> (0, 0, 0, 0, 0) (*This can't happen! *)



(* Gets the best resource to play monopoly on, just by calculating
	which resource you'd get the most of. *)
let get_best_mono_resource col plist = 
	let b = ref 0 and w = ref 0 and o = ref 0 and g = ref 0 and l = ref 0 in
	let () = List.fold_left (fun () (pc, ((bp,wp,op,gp, lp), _), _) ->
		if (pc = col) then () else b:=(!b+bp); w:= (!w+wp); o:=(!o+op);
		g:=(!g+gp); l:=(!l+lp)) () plist in 
	let ls = [(Brick, !b); (Wool, !w); (Ore, !o); (Grain, !g); (Lumber, !l)] in
	let ls = List.sort (fun (_, a) (_, b) -> b - a) ls in
	match ls with
		| [] -> Lumber (* won't happen *)
		| (typ, _)::_ -> typ




(* We want to build a town, What is a good next move?
Should we try to build it right away, or should we build a road 
to a better location first? Should we initiate a trade? ... *)
let town_move ((inters, roads) : structures) res_tiles board (b,w,o,g,l) color : objective = 
	let weighted_locs = sort_locs inters board res_tiles in
	let nearby_options = get_near_empty (inters, roads) color true in
	let nearby_options = if (List.length nearby_options) = 0 then 
		get_near_empty (inters, roads) color false else nearby_options in
	let rec choose_move wl : objective = 
		match wl with
		| [] -> NA
		| (_, h, _)::t -> 
			let valid_options = List.filter (fun (i, _, _) -> i = h) nearby_options in
			begin match valid_options with
			  | [] -> choose_move t
			  | (loc, dis, trail)::_ -> let () = print_endline (string_of_int (List.length trail)) in 
			  	let () = List.fold_left (fun acc i -> print_endline (string_of_int i)) () trail in 
			  	trail_to_move (inters, roads) color loc trail
			  end in
	choose_move weighted_locs

 (* We want to build a city, what is a good town to choose to upgrade? *)
let city_move (inters, roads) res_tiles board color : objective = 
	let towns = List.mapi (fun i set -> (set, i)) inters in
	let my_towns = List.fold_left (fun acc v -> match v with
		| ((Some (tcol, tsettle)), i) -> if (tcol = color && tsettle = Town) then i::acc else acc
		| (None, _) -> acc
	) [] towns in
	let my_towns_weighted = List.map (fun i -> (i, (weight_loc None i board res_tiles inters))) my_towns in
	let my_towns_sorted = List.sort (fun (_, a) (_, b) -> b - a) my_towns_weighted in 
	match my_towns_sorted with
		| (h, _)::_ -> OCity h
		| _ -> NA

(* Given the resources we have and those we need,
	try to get some of the ones we need by whatever means
	are available (sea trade, player trade, playing cards ,etc) *)
let try_for_res hexes (inters, rlist) robberloc needo color plist goal:  strategy =
	let (_, (have, cards), _) = get_player color plist in
	let cards = match cards with | Hidden _ -> [] | Reveal l -> l in
	let (b, w, o, g, l) = have in 
	let (bn, wn, on, gn, ln) = needo in
	let need = res_needed have needo in
	let (res_to_get : resource option) = match need with
		| [] -> None
		| (h, _)::t -> Some h in
	let need_sort = List.sort (fun (_, a) (_, b) -> b - a) need in 

	(* If we have some cards, see if they would be useful. *)
	let rb_strat = if (List.mem RoadBuilding cards) then
		let num_towns = get_num_settles color inters Town in
		let num_cities = get_num_settles color inters City in
		if (needo=cCOST_ROAD) then match goal with
			| ORoad (p1, p2) -> SPlayRoadBuild (p1, p2)
			| _ -> NoStrategy (* should never happen! *)
		else if (num_towns >= cMAX_TOWNS_PER_PLAYER && num_cities >= cMAX_CITIES_PER_PLAYER
			&& (get_num_roads color rlist) <= cMAX_ROADS_PER_PLAYER) then 
		(* insert logic for building road after main construction phase here *) NoStrategy
	else NoStrategy else NoStrategy in
	match rb_strat with
		| SPlayRoadBuild (p1, p2) -> SPlayRoadBuild (p1, p2)
		| _ ->

	if (List.mem YearOfPlenty cards) then
		let ((r1, num1), left) = match need_sort with
			| [] -> ((Brick, 1), [])
			| (r, n)::t -> ((r, n), t) in
		match left with
			| [] -> SPlayYoP (r1, r1)
			| (r2, _)::_ -> SPlayYoP (r1, r2)
	else 
	if (List.mem Monopoly cards) then
		SPlayMonopoly (get_best_mono_resource color plist)
	else 
	if (List.mem Knight cards) then
		SPlayKnight (get_robber_move hexes inters color robberloc plist needo)
	else

	(* If building a card doesn't really hurt our cause, do it. *)
	if enough_res have cCOST_CARD then
		SBuildCard else

	(* Check for usefulness of maritime trade. *)
	let trade_res = if (b >= bn + 4) then Some Brick 
			   else if (w >= wn + 4) then Some Wool 
			   else if (o >= on + 4) then Some Ore
			   else if (g >= gn + 4) then Some Grain
			   else if (l >= ln + 4) then Some Lumber
			   else None in
	match trade_res with
		| Some r_have -> begin match res_to_get with
			| Some r_want -> SMaritimeTrade (r_have, r_want)
			| None -> NoStrategy (* We shouldn't have called this function if we don't need a resource! *)
		 end
		| None ->

	(* We have tried everything, just give up. *)
	NoStrategy

