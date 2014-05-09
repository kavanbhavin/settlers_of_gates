open Constant
open Definition
open Util

type own_point = CanBuild of int * int | DistanceToNearest of (int * int * int) 

(*first two ints represent road in both cases*)
type own_road = RoadCanBuild of (int * int * int) | RoadDistanceToNearest of (int * int * int * int)

let board_points : int list= let rec helper n acc = 
	if n = (-1)
	then acc 
	else helper (n-1) (n::acc) 
in helper (cNUM_POINTS-1) [] 

let rec remove_duplicates ls = 
	let ls' = List.sort compare ls
in match ls' with 
	| a::b::tl -> if a = b then (remove_duplicates (a::tl)) else a::(remove_duplicates (b::tl))
	| _ -> ls'

(* uses an array, that is indexed by points, to compute road distance between two points. 
The array is set to true for every point that has been seen already. *)
let distance_between_points_array p1 p2 = 
	let distance = (let rec helper p acc distance = 
		if acc.(p) = true then distance
	else let lop = ref [] in 
	for i=0 to ((Array.length acc)-1) do 
	if acc.(i) = true 
	then lop := (adjacent_points i) @ !lop
else ()
done; (List.iter (fun element -> acc.(element) <- true) !lop); 
helper p acc (distance+1)
in let init_array = Array.make cNUM_POINTS false in (init_array.(p2) <- true); 
helper p1 init_array 0  ) in if distance < cNUM_POINTS then distance else failwith "bogus distance"


(* extremely slow (but functional) implementation of distance between points function *)
let distance_between_points p1 p2 =
	try (let rec helper p lop distance = 
		if List.mem p lop 
		then distance
		else helper p (remove_duplicates (List.flatten (List.map adjacent_points lop))) (distance+1)
	in helper p1 [p2] 1) with _ -> failwith "failure in distance in list points"


let memoized_distance = 
	let h = Hashtbl.create cNUM_POINTS 
in fun (p1, p2) -> 	let smaller = min p1 p2 
in let bigger = max p1 p2 in
		if Hashtbl.mem h (smaller, bigger)
		then Hashtbl.find h (smaller, bigger)
	else let distance = distance_between_points_array smaller bigger
in (Hashtbl.add h (smaller, bigger) distance); distance


(* Returns true if there exists a settlement
    < 2 road lengths away from point. *)
  let settle_one_away point inters: bool = 
    try (let neighbors = adjacent_points point in
    List.fold_left (fun acc v ->
      acc || (match (List.nth inters v) with
        | None -> false
        | Some _ -> true)) false neighbors) with _ -> failwith "failure in settle_one_away"

let should_build_road (point : point) (inters : intersection list) : bool = let point_is_empty = match (List.nth inters point) with 
				| None -> true 
				| _ -> false 
			in point_is_empty && (not(settle_one_away point inters))

let convert_to_our_type (inters :intersection list) : own_point list= 
	List.map (fun (i : int)-> 
		if (should_build_road i inters) 
		then CanBuild (i, 0) 
	else DistanceToNearest (i,0,0)) board_points

let only_free_and_buildable_points inters : own_point list = 
	List.filter (fun a -> 
	match a with 
	| CanBuild (i, _) -> 
		begin match List.nth inters i with 
				| None -> true
				| _ -> false
			end
	| _ -> false ) (convert_to_our_type inters)

let fill_in_distances inters : own_point list = 
	let ofbp = only_free_and_buildable_points inters in 
	List.rev (List.fold_left (fun (acc : own_point list) (ele : own_point) -> 
	match ele with 
	| DistanceToNearest (i, _, _) -> let distances : int list = 
			List.map (fun a -> 
				begin match a with 
					| CanBuild (b, _) -> (memoized_distance (b, i)) 
					| _ -> failwith "not can build in free and buildable?"
				end
	) ofbp
		in let (min_distance, second_min_distance) : int * int = (List.fold_left (fun (min1, min2) element -> 
			if element < min1 
			then (element, min1)
			else if element < min2
			then (min1, min2)
else (min1, min2)) (cNUM_POINTS, cNUM_POINTS) distances)
in ((DistanceToNearest (i, min_distance, second_min_distance))::acc)
	| CanBuild (i, _) ->let distances = 
		List.map (fun a -> 
			begin match a with 
				| CanBuild (b, _) -> (memoized_distance (b, i))
				| _ -> failwith "not can build in free and buildable?"
			end 
	) ofbp
	in let min_distance = (List.fold_left (fun min element -> 
		if element < min 
		then element
		else min)) (cNUM_POINTS) distances 
	in ((CanBuild (i, min_distance))::acc)) [] (convert_to_our_type inters))

let sorted_distances inters= let fid = fill_in_distances inters 
in let check_order = let bools = List.mapi (fun i a -> match a with 
					| CanBuild (j, _) 
					| DistanceToNearest (j, _, _) -> i=j ) fid
in List.fold_left (fun acc ele -> acc && ele) true bools in if check_order then ((*Printf.printf "order was correct"; *) fid) 
else failwith "incorrect order" (*((Printf.printf "order was incorrect"); List.sort (fun a b -> 
	begin match (a,b) with 
		| (CanBuild i, CanBuild j) 
		| (CanBuild i, DistanceToNearest (j, _, _ )) 
		| ((DistanceToNearest (i, _, _), CanBuild j))
		| ((DistanceToNearest (i, _, _), DistanceToNearest(j, _, _))) -> compare i j
	end ) fid)
*)

(*Given a point, returns the list of possible roads in own type.*)
let best_road_from_point inters rlist p : own_road list =  
	try (let already_built_roads_from_point = List.fold_left (fun acc (_, (p1,p2)) ->
	 if p1=p then p2::acc else if p2=p then p1::acc else acc) [] rlist 
in let s = (sorted_distances inters) 
in let our_type = List.map (fun a -> List.nth s a) (adjacent_points p) 
in let our_type_with_built_roads_removed = List.filter (fun a -> match a with 
			| CanBuild (i, _) 
			| DistanceToNearest (i, _ , _ ) -> not(List.mem i already_built_roads_from_point)) our_type
	in List.map (fun a -> match a with 
		| CanBuild (i, j) -> RoadCanBuild (p,i, j)
		| DistanceToNearest (i,j,k) -> RoadDistanceToNearest (p,i,j,k) ) our_type_with_built_roads_removed)
with _ -> (Printf.printf "invalid point %d" p); failwith "asf"
	(*let asd = List.fold_left (fun (acc : own_point) (ele : own_point) -> 
	match (acc, ele) with 
		| (CanBuild (i,j), CanBuild (k,l)) -> if j <l then acc else ele  
		| (CanBuild (i,j), DistanceToNearest (k, l, m )) -> if j < (l+m) then acc else ele 
		| ((DistanceToNearest (k, l, m), CanBuild (i,j))) -> if j < (l+m) then acc else ele 
		| ((DistanceToNearest (_, d1, d2), DistanceToNearest(_, d3, d4))) -> 
		if ((1.5 *. (float_of_int d1)) +. (float_of_int d2)) <= ((1.5 *. (float_of_int d3)) +. (float_of_int d4)) then acc else ele) 
	(DistanceToNearest (1, cNUM_POINTS, cNUM_POINTS)) our_type_with_built_roads_removed
in match asd with 
| CanBuild (i, _) -> i 
| DistanceToNearest (j, _, _) -> j*)
let road_exists (r1, r2) rlist : bool = 
  	List.exists (fun (r1', r2') -> ((r1 = r1' && r2 = r2') || (r1 = r2' && r2 = r1'))) rlist
  let real_road_exists (r1, r2) rlist : bool = 
  	List.exists (fun (_, (r1', r2')) -> ((r1 = r1' && r2 = r2') || (r1 = r2' && r2 = r1'))) rlist
(* Returns a list of all possible roads that color can build. *)
let all_possible_roads ((inters, roads) : structures) color : (int * int) list = 
	let settlesi = (List.mapi (fun i valu -> (valu, i)) inters) in
	(* Get all roads possible on the map. *)
	let all_roads = List.fold_left (fun acc (_, i) ->
		let adj = adjacent_points i in
		(List.fold_left (fun acc' b ->
			if (real_road_exists (i, b) roads)  || (road_exists (i,b) acc') then acc'
			else (i, b)::acc') acc adj)) [] settlesi in
	(* Is a town either owned by us or empty? *)
	let town_checks n = match ((List.nth inters n) : intersection) with
		| None -> true
		| Some (c, _) -> c = color in
	(* Filter all possible roads to just the ones we can build. *)
	let possible_roads = List.filter (fun (a, b) ->
		(List.fold_left (fun acc (r_c, (r_s, r_e)) -> 
    		acc || (((r_s = a) || (r_e = a)) && r_c = color && (town_checks a))) false roads  || 
		List.fold_left (fun acc (r_c, (r_s, r_e)) -> 
    		acc || (((r_s = b) || (r_e = b)) && r_c = color && (town_checks b))) false roads)) all_roads
	in
	possible_roads

let remove_other_roads (possible_roads : own_road list) (rlist: road list) own_color inters = 
	let all_roads = all_possible_roads (inters, rlist) own_color
in  List.filter (fun a -> 
	match a with 
		| RoadCanBuild (p1, p2, _) 
		| RoadDistanceToNearest (p1, p2, _, _) -> (List.mem (p1,p2) all_roads || List.mem (p2,p1) all_roads) ) possible_roads
(* simply returns the best road using this density based strategy*)
let best_road inters rlist own_color : road= 
	let my_roads = List.filter (fun (color, line) -> color = own_color) rlist
	in let points_to_build_from = remove_duplicates (List.fold_left (fun acc (color, (p1,p2)) -> 
	p1::p2::acc) [] my_roads)
	in let possible_roads = remove_other_roads (List.flatten (List.map (best_road_from_point inters rlist) points_to_build_from)) rlist own_color inters
	in let asd = List.fold_left (fun (best_road : own_road) (ele : own_road) -> 
	match (best_road, ele) with 
		| (RoadCanBuild (_,_,j), RoadCanBuild (_,_,l)) -> if j <l then best_road else ele  
		| (RoadCanBuild (_,_,j), RoadDistanceToNearest (_, _, l, m) ) -> if j < (l+m) then best_road else ele 
		| ((RoadDistanceToNearest (_, _, l, m), RoadCanBuild (_, _, j))) -> if j < (l+m) then best_road else ele 
		| ((RoadDistanceToNearest (_, _, d1, d2), RoadDistanceToNearest(_, _, d3, d4))) -> 
		if ((1.5 *. (float_of_int d1)) +. (float_of_int d2)) <= ((1.5 *. (float_of_int d3)) +. (float_of_int d4)) then best_road else ele) 
	(RoadDistanceToNearest ((-100), 1, cNUM_POINTS, cNUM_POINTS)) possible_roads
in match asd with 
	| RoadCanBuild (p1, p2, _) -> (own_color, (p1, p2))
	| RoadDistanceToNearest ((-100), 1, _, _) -> begin match all_possible_roads (inters, rlist) own_color with 
														| hd::tl -> (Printf.printf "road was possible but best_road didn't find it"); (own_color, hd)
														| [] -> (Printf.printf "no roads left"); (own_color, (1,1))
													end 
	| RoadDistanceToNearest (p1, p2, _, _) -> (own_color, (p1, p2)) 

(* settlement, settlement location, weight sorted so first one is the best *)

(* returns the best points on the board according to this density based strategy *)

let best_points_sorted inters rlist own_color : point list = let entire_map = sorted_distances inters 
	in let entire_map_sorted = List.sort (fun a b-> 
	match (a,b) with 
		| (CanBuild (i,j), CanBuild (k,l)) -> if j  = l then  0 else if j<l then (-1) else 1 
		| (CanBuild (i,j), DistanceToNearest (k, l, m )) -> if j = (l+m) then 0 else if j < (l+m) then (-1) else 1
		| ((DistanceToNearest (k, l, m), CanBuild (i,j))) -> if j = (l+m) then 0 else if j < (l+m) then (-1) else 1
		| ((DistanceToNearest (_, d1, d2), DistanceToNearest(_, d3, d4))) -> 
		if ((1.5 *. (float_of_int d1)) +. (float_of_int d2)) <= ((1.5 *. (float_of_int d3)) +. (float_of_int d4)) then (-1) else 1) entire_map
in List.map (fun a -> 
	match a with 
	| CanBuild (i,_) 
	| DistanceToNearest (i, _ , _ )-> i) entire_map_sorted

(*returns weights based on density in a list indexed by points *)
let best_points_weighted inters rlist own_color : float list = let entire_map = sorted_distances inters 
	in List.map (fun a -> 
	match a with 
		| CanBuild (i,j) -> (float_of_int j)
		| DistanceToNearest (k, d1, d2) -> (1.5 *. (float_of_int d1)) +. (float_of_int d2)) entire_map

(*from bot_util*)

type phase = Early | Middle | Late 
  type objective = NA | OTown of int | OCity of int | OCard | ORoad of int * int
  type to_buy = RoadBuy | TownBuy | CityBuy | CardBuy
  type strategy = SPlayerTrade of int * cost * cost | SMaritimeTrade of resource * resource | SBuildCard | NoStrategy
  	| SPlayYoP of resource * resource | SPlayMonopoly of resource | SPlayKnight of robbermove | SPlayRoadBuild of road * road option
  	| SBuildRoad of road | SOfferTrade of trade

(*returns the number of points generating the a resource on the board*)  	
let get_num_each_resource hex_list : cost = 
	let resources = List.map (fun (t,r) -> resource_of_terrain t) hex_list
in List.fold_left (fun (b,w,o,g,l) ele-> 
	match ele with  
		| Some Brick -> (b+1, w, o, g, l)
		| Some Wool -> (b, w+1, o, g, l)
		| Some Ore -> (b, w, o+1, g, l)
		| Some Grain -> (b, w, o, g+1, l)
		| Some Lumber -> (b, w, o, g, l+1)
		| None -> (b, w, o, g, l)) (0,0,0,0,0) resources

(* Returns "player col owns a road with one point
    on point." *)
let own_road_with_point point col roads : bool = 
  List.fold_left (fun acc (r_c, (r_s, r_e)) -> 
    acc || ((r_s = point || r_e = point) && r_c = col)) false roads

   (* Helper function for longest road that filters first. *)
  let longest_road' c roads inters = 
    let roads = List.filter (fun (col, _) -> col = c) roads in
    longest_road c roads inters

  let road_exists (r1, r2) rlist : bool = 
  	List.exists (fun (r1', r2') -> ((r1 = r1' && r2 = r2') || (r1 = r2' && r2 = r1'))) rlist
  let real_road_exists (r1, r2) rlist : bool = 
  	List.exists (fun (_, (r1', r2')) -> ((r1 = r1' && r2 = r2') || (r1 = r2' && r2 = r1'))) rlist

(* Returns a list of all possible roads that color can build. *)
let all_possible_roads ((inters, roads) : structures) color : (int * int) list = 
	let settlesi = (List.mapi (fun i valu -> (valu, i)) inters) in
	(* Get all roads possible on the map. *)
	let all_roads = List.fold_left (fun acc (_, i) ->
		let adj = adjacent_points i in
		(List.fold_left (fun acc' b ->
			if (real_road_exists (i, b) roads)  || (road_exists (i,b) acc') then acc'
			else (i, b)::acc') acc adj)) [] settlesi in
	(* Is a town either owned by us or empty? *)
	let town_checks n = match ((List.nth inters n) : intersection) with
		| None -> true
		| Some (c, _) -> c = color in
	(* Filter all possible roads to just the ones we can build. *)
	let possible_roads = List.filter (fun (a, b) ->
		(List.fold_left (fun acc (r_c, (r_s, r_e)) -> 
    		acc || (((r_s = a) || (r_e = a)) && r_c = color && (town_checks a))) false roads  || 
		List.fold_left (fun acc (r_c, (r_s, r_e)) -> 
    		acc || (((r_s = b) || (r_e = b)) && r_c = color && (town_checks b))) false roads)) all_roads
	in
	possible_roads

(* Return whether or not building a road would increase our longest road.
	If so, Some of that road. Otherwise None. *)
let extend_road color (inters, roads) : road option = 
	let long_road = longest_road' color roads inters in 
	let poss_roads = all_possible_roads (inters, roads) color in
	let weighted_roads = List.map (fun l -> 
		let new_roads = (color,l)::roads in 
		let weight = (longest_road' color new_roads inters) - long_road in 
		((color, l), weight)) poss_roads in 
	let sorted_roads = List.sort (fun (_, a) (_, b) -> b - a) weighted_roads in
	match sorted_roads with
		| [] -> None
		| (r, w)::_ -> if (w <= 0) then None else Some r

	

(* Returns a certain player from a player list.
THIS FUNCTION FAILS IF THE PLAYER ISN'T IN THE LIST. *)
let get_player color plist =
	try List.find (fun (c, _, _) -> c = color) plist with
	Not_found -> failwith "get_player : Player not found!"

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


  let get_num_trophy_points col plist = 
    let (_, _, (_, longroad, largearmy)) = get_player col plist in
    let count = 0 in 
    let count = if (longroad) then count + cVP_LONGEST_ROAD else count in
    let count = if (largearmy) then count + cVP_LARGEST_ARMY else count in
    count

  (* Gets a player's number of victory points. *)
  let get_num_vp col plist (inters, roads) : int = 
    let town_points = cVP_TOWN * (get_num_settles col inters Town) in
    let city_points = cVP_CITY * (get_num_settles col inters City) in
    let card_points = 0 in
    let troph_points = get_num_trophy_points col plist in
    town_points + city_points + card_points + troph_points

  (* Returns the winner of the game, or None if it isn't over yet. *)
  let get_winner plist (inters, roads) : color option = 
    List.fold_left (fun acc (col, hand, troph) ->
      if (get_num_vp col plist (inters, roads)) >= cWIN_CONDITION then
      (Some col) else acc) None plist


let ratio_of_settlements (inters : intersection list) : float = 
	let num_settlements = List.fold_left (fun acc inter -> 
		match inter with 
		| None -> acc 
		| Some (_) -> acc+1) 0 inters
		in (float_of_int num_settlements) /. (float_of_int cNUM_POINTS) 

let close_to_win_vp = 7
let any_player_close_to_winning plist structures = List.fold_left (fun acc (color, hand, trophy) -> 
	if get_num_vp color plist structures >= close_to_win_vp then true else acc) false plist 

let change_phase_ratio : float = 0.35

let which_phase (inters, rlist) plist = if any_player_close_to_winning plist (inters, rlist)
	then Late else if (ratio_of_settlements inters) > change_phase_ratio then Middle else Early
let is_robber_on_mine robber (inters : intersection list) own_color: bool = 
	let corners = piece_corners robber in 
	List.fold_left (fun acc point -> 
	match List.nth inters point with
		| Some (color, _) -> color=own_color || acc
		| None -> acc ) false corners
let phase_is_late structures plist = (which_phase structures plist) = Late 


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
	let (a,b,c,d,e,f,g) = (*(1,2,3,4,5,6,0) in
	let (a,b,c,d,e,f,g) = if (import_factor = 1) then (a,b,c,d,e,f,g) else
	if (import_factor = 2) then*) (1,2,4,7,11,16,0) (*else 
	if (import_factor = 3) then (1, 5, 12, 22, 35, 51, 0) else (a,b,c,d,e,f,g) *)
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
	List.fold_left (fun acc (v, _) ->
		if (v = res) then acc+1 else acc) 0 reslist

let num_dice_owned roll reslist = 
	List.fold_left (fun acc (_, r) ->
		if (r = roll) then acc+1 else acc) 0 reslist

let weight_num_owned (res, roll) reslist = 
	(num_owned res reslist) * 4 + (num_dice_owned roll reslist) * 0

(*puts more weight on grain and ore*)
let weight_loc1 settle num board res_owned inters= 	
	let ((hexlist, _), _, _, _, _) = board in 
	match settle with
		| Some _ -> 0
		| None ->
	if (settle_one_away num inters) then 0 else
	let (bnum, wnum, onum, gnum, lnum) = get_num_each_resource hexlist in
	let hexes_gained = adjacent_pieces num in
	fst (List.fold_left (fun (acc, curlist) hex_gained ->
		try (let (terrain,roll) = List.nth hexlist hex_gained in
	 let (acc', div_factor, res, import_factor) =
		 let res_owned' = curlist@res_owned in
		 match (resource_of_terrain terrain) with
			| None -> (-1, 1, [], 0)
			| Some Grain -> ((gnum*25), 1 + (weight_num_owned (Grain, roll) res_owned'), [Grain, roll], 3)
			| Some Lumber -> ((lnum*12), 1 + (weight_num_owned (Lumber, roll) res_owned'), [Lumber, roll], 2)
			| Some Brick -> ((bnum*13), 1 + (weight_num_owned (Brick, roll) res_owned'), [Brick, roll], 2)
			| Some Wool -> ((wnum*17), 1 + (weight_num_owned (Wool, roll) res_owned'), [Wool, roll], 1)
			| Some Ore -> ((onum*30), 1 + (weight_num_owned (Ore, roll) res_owned'), [Ore, roll], 3) in
		let acc' = acc' * (expected_of_36 roll import_factor) / div_factor in
		(acc + acc', res@curlist)
	) with _ -> (acc, curlist)) (0, []) hexes_gained)
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
	let (bnum, wnum, onum, gnum, lnum) = get_num_each_resource hexlist 
in fst (List.fold_left (fun (acc, curlist) hex_gained ->
		try (let (terrain,roll) = List.nth hexlist hex_gained in
		let (acc', div_factor, res, import_factor) =
		 let res_owned' = curlist@res_owned in
		 match (resource_of_terrain terrain) with
			| None -> (-1, 1, [], 0)
			| Some Grain -> ((gnum*17), 1 + (weight_num_owned (Grain, roll) res_owned'), [Grain, roll], 2)
			| Some Lumber -> ((lnum*33), 1 + (weight_num_owned (Lumber, roll) res_owned'), [Lumber, roll], 3)
			| Some Brick -> ((bnum*33), 1 + (weight_num_owned (Brick, roll) res_owned'), [Brick, roll], 3)
			| Some Wool -> ((wnum*17), 1 + (weight_num_owned (Wool, roll) res_owned'), [Wool, roll], 2)
			| Some Ore -> ((onum*12), 1 + (weight_num_owned (Ore, roll) res_owned'), [Ore, roll], 1) in
		let acc' = acc' * (expected_of_36 roll import_factor) / div_factor in
		(acc + acc', res@curlist)
	) with _ -> (acc, curlist)) (0, []) hexes_gained)

(* Adds a resource to the list of resource tiles we own. *)
let add_res loc reslist board = 
	let ((hexlist, _), _, _, _, _) = board in 
	let hexes_gained = adjacent_pieces loc in
	let to_add = List.fold_left (fun acc v -> try
		let (terrain, roll) = List.nth hexlist v in
		match (resource_of_terrain terrain) with
			| None -> acc
			| Some x -> (x,roll)::acc
	with _ -> acc) [] hexes_gained
	in reslist := to_add@(!reslist)

(* Sort intersections in order of increasing desirability. *)
let sort_locs inters board res_tiles plist = 
	let (_, structures, _ , _ ,_ ) = board in 
	match which_phase structures plist with 
	| Early -> 	let weighted_inters = List.mapi (fun n inter ->
      (inter, n, (weight_loc inter n board res_tiles inters))) inters in
    List.sort (fun (_, _, w1) (_,_, w2) -> w2 - w1) weighted_inters
    | _ -> 	let weighted_inters = List.mapi (fun n inter ->
      (inter, n, (weight_loc1 inter n board res_tiles inters))) inters in
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
	(* let () = print_endline ("owned settles: "^(string_of_int (List.length owned_settles))) in *)
	let garbage = List.map (fun (i, _, _) -> (i, 0, [])) owned_settles in
	let (one_away, garbage) = extend_path (inters, roads) owned_settles garbage first_done color in
	(* let () = print_endline ("one away: "^(string_of_int (List.length one_away))) in *)
	let (two_away, garbage) = extend_path (inters, roads) one_away garbage false color in
	let two_away = List.filter (fun (i, _, _) -> not (settle_one_away i inters)) two_away in
	(* let () = print_endline ("two away: "^(string_of_int (List.length two_away))) in *)
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

(* Given a settlement list, and an index into that list, determine if the player
	living there has resources that can be stolen. *)
let has_resources settles loc plist = 
	try let settle = List.nth settles loc in match settle with
		| None -> false
		| Some (color, _) -> let res = get_res color plist in
			let (b,w,o,g,l) = res in 
			b > 0 || w > 0 || o > 0 || g > 0 || l > 0
	with  _ -> false


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
		| NoRoad ->  ORoad (start_loc, between_loc)
		| Us -> trail_to_move (inters, roads) color goal_loc (between_loc::n2)
		| Them -> NA
	end

(* We want to build a town, What is a good road to choose?
	Parallels town_move so that we next try and build a town
	off of one of these roads.  *)
let get_init_road ((inters, roads) : structures) res_tiles board color loc plist : int * int =
	(* Update inters so that it only contains the player's current location. *)
	let inters = List.mapi (fun i v -> if (i = loc) then Some (color, Town) else
		match v with
			| None -> None
			| Some (col, t) -> if (col = color) then None else Some (col, t)) inters in
	let weighted_locs = sort_locs inters board res_tiles plist in
	let nearby_options = get_near_empty (inters, roads) color false in
	let rec choose_move wl : objective = 
		match wl with
		| [] -> NA
		| (_, h, _)::t -> 
			let valid_options = List.filter (fun (i, _, _) -> i = h) nearby_options in
			begin match valid_options with
			  | [] -> choose_move t
			  | (loc, dis, trail)::_ -> 
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

 let color_has_resource color plist = 
	List.fold_left (fun acc (color', (inventory, cards), trophies) -> 
	if color'=color then (sum_cost inventory) > 0 || acc 
	else acc) false plist

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
 	let temp = int_of_float float_scaled in 
 	let temp = if (color_has_resource color plist) then temp+1 else temp in
 	temp

 (* Pick a robber move to play. *)
 let get_robber_move hexes inters color robber_loc plist need=
 	let hexesi = List.mapi (fun i hex -> i) hexes in
 	(* Try to find a spot that isn't on our land and lets us steal. *)
 	let goal = List.fold_left (fun acc v ->
 			if (v = robber_loc) then acc else
 			let adjacent_locs = piece_corners v in
 			match (List.fold_left (fun (acc, blocks_us) v' ->
 				if blocks_us then ([], true) else
 				match (List.nth inters v') with
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
		let goal = List.map (fun (loc, col) -> (loc, col, (get_prob_pick_card col plist need))) goal in
		let goal = List.sort (fun (_,_,a) (_, _, b) -> b-a) goal in 
		begin match goal with
			| (loc, col, w)::_ -> if (color_has_resource col plist) then (loc, Some col)  else  (loc, None) 
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
		(* let () = print_endline ("need discard: "^(string_of_int num_to_discard)) in *)
		let sorted_list = List.sort (fun a b -> b - a) (cost_to_list leftovers) in match sorted_list with
			| h::_ -> let new_hand = choose_from_leftovers (cost_to_list leftovers) h 0 in 
				(* let (b,w,o,g,l) = new_hand in *)
				(* let () = print_endline ("temp: "^(string_of_int b)^(string_of_int w)^(string_of_int o)^(string_of_int g)^(string_of_int l)) in *)
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
let town_move ((inters, roads) : structures) res_tiles board (b,w,o,g,l) color plist : objective = 
	let weighted_locs = sort_locs inters board res_tiles plist in
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
			  | (loc, dis, trail)::_ -> 
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

(* Given a player's color, do they own a settlement
  on either end of a line? (used mostly for ports)
    Precondition: line is valid on the map *)
let owns_port (p1, p2) col inters = 
  let s1 = List.nth inters p1 and s2 = List.nth inters p2 in
  let owns_p1 = match s1 with
    | None -> false
    | Some (c, _) -> (c = col) and
      owns_p2 = match s2 with
    | None -> false
    | Some (c, _) -> (c = col) in
  owns_p1 || owns_p2

(* Calculate the best ratio the player can
use to trade in the given resource. Must be either
2, 3, or 4. *)
let calc_best_ratio (ports : port list) inters col : ratio = 
	List.fold_left (fun best_ratio (line, ratio, port_res) ->
		let right_type = match port_res with
			| Any -> true 
			| PortResource port_res -> false in
		if (owns_port line col inters) && ratio < best_ratio && right_type then
		ratio else best_ratio
	) cMARITIME_DEFAULT_RATIO ports

let get_num_vp_cards col plist = 
    let (col, (inv, cards), trophs) = get_player col plist in
    let hand = match cards with
     | Hidden _ -> failwith "Get Num VP Cards: Cards Hidden!"
     | Reveal ls -> ls in
    List.fold_left (fun acc v ->  if (v = Knight) then acc+1
      else acc) 0 hand

  let get_num_trophy_points col plist = 
    let (_, _, (_, longroad, largearmy)) = get_player col plist in
    let count = 0 in 
    let count = if (longroad) then count + cVP_LONGEST_ROAD else count in
    let count = if (largearmy) then count + cVP_LARGEST_ARMY else count in
    count

  (* Gets a player's number of victory points. *)
  let get_num_vp_us col plist (inters, roads) : int = 
    let town_points = cVP_TOWN * (get_num_settles col inters Town) in
    let city_points = cVP_CITY * (get_num_settles col inters City) in
    let card_points = cVP_CARD * (get_num_vp_cards col plist) in
    let troph_points = get_num_trophy_points col plist in
    town_points + city_points + card_points + troph_points

let have_longest_road plist (inters, roads) color = 
	let proads = List.map (fun (pcol, phand, ptroph) -> 
		let numroads = longest_road' pcol roads inters in
		((pcol, phand, ptroph), numroads)) plist in 
	let winner = List.fold_left (fun acc ((pcol, _, _), numroads) ->
		match acc with
			| None -> if (numroads >= cMIN_LONGEST_ROAD) then
				Some (pcol, numroads) else None
			| Some (pcolold, numroadsold) ->
				if (numroads > numroadsold) then
				Some (pcol, numroads) else Some (pcolold, numroadsold)) None proads in
	match winner with
		| None -> false 
		| Some (col, _) -> col = color

let have_largest_army plist (inters, roads) color = 
	let winner = List.fold_left (fun acc (pcol, _, (knights, _, _)) ->
		match acc with
			| None -> if (knights >= cMIN_LARGEST_ARMY) then
				Some (pcol, knights) else None
			| Some (pcolold, sizearmyold) ->
				if (knights > sizearmyold) then
				Some (pcol, knights) else Some (pcolold, sizearmyold)) None plist in
	match winner with
		| None -> false 
		| Some (col, _) -> col = color


let delta_longest_road plist (inters, roads) color = 
	let my_road_length = longest_road' color roads inters in 
	let proads = List.map (fun (pcol, phand, ptroph) -> 
		let numroads = longest_road' pcol roads inters in
		((pcol, phand, ptroph), numroads)) plist in 
	let winner = List.fold_left (fun acc ((pcol, _, _), numroads) ->
		match acc with
			| None -> if (numroads >= cMIN_LONGEST_ROAD) then
				Some (pcol, numroads) else None
			| Some (pcolold, numroadsold) ->
				if (numroads > numroadsold) then
				Some (pcol, numroads) else Some (pcolold, numroadsold)) None proads in
	match winner with
		| None -> cMIN_LONGEST_ROAD-my_road_length 
		| Some (_, l) -> l - my_road_length

let delta_largest_army plist (inters, roads) color = 
	let (_, _, (my_knights, _, _)) = get_player color plist in
	let winner = List.fold_left (fun acc (pcol, _, (knights, _, _)) ->
		match acc with
			| None -> if (knights >= cMIN_LARGEST_ARMY) then
				Some (pcol, knights) else None
			| Some (pcolold, sizearmyold) ->
				if (knights > sizearmyold) then
				Some (pcol, knights) else Some (pcolold, sizearmyold)) None plist in
	match winner with
		| None -> cMIN_LARGEST_ARMY - my_knights 
		| Some (_, num) -> num-my_knights

let get_trade_info turn = 
	match turn.pendingtrade with
		| None -> ((0,0,0,0,0), (0,0,0,0,0), Blue) (* should never happen *)
		| Some (_, get, lose) -> (get, lose, turn.active)

(* Similar logic to discard, but we are trying to find resources to offer in a trade,
	so the num_to_discard is more of a soft guideline rather than a hard minimum. *)
let get_res_to_offer curr goal num_to_discard = 
 	let leftovers = subtract_res curr goal in 
 	let leftovers = normalize leftovers in
 	let rec choose_from_leftovers ls largest_el num_removed =
 		let num_removed = ref num_removed in  
 		let ls' = List.map (fun x -> if (x = largest_el && !num_removed < num_to_discard)
 			then let () = (num_removed:= (!num_removed + 1)) in x-1 else x) ls in
 		if (!num_removed < num_to_discard && largest_el <> 0) then choose_from_leftovers ls' (largest_el - 1) !num_removed
		else list_to_cost ls' in 
	let sorted_list = List.sort (fun a b -> b - a) (cost_to_list leftovers) in match sorted_list with
		| h::_ -> let new_hand = choose_from_leftovers (cost_to_list leftovers) h 0 in 
			subtract_res leftovers new_hand
		| [] -> (0, 0, 0, 0, 0) (* Should never happen. *)

(* Try to come up with a trade that might be helpful. 
	Do not try with anyone who is beating us (as visible). Try trading at 2x ratio.
	Find a player who has the most resources we need, and try bargaining for them with 
	resources we don't need. *)
let suggest_trade need plist trades_made_o our_color (inters, roads): trade option = 
	let trades_made = !trades_made_o in 
	let our_vp = get_num_vp our_color plist (inters, roads) in 
	let our_res = get_res our_color plist in 
	let res_to_go = subtract_res need our_res in
	let res_to_go = normalize res_to_go in 
	let (num_gained, trad) = List.fold_left (fun (num_best, tr) (col, _, _) ->
		let default = (num_best, tr) in 
		if (col = our_color) then default else
		let their_vp = get_num_vp col plist (inters, roads) in 
		if (their_vp > our_vp) then default else
		let their_res = get_res col plist in 
		let ours_with_all_theirs = add_res_vals our_res their_res in 
		let would_be_res_to_go = subtract_res need ours_with_all_theirs in 
		let would_be_res_to_go = normalize would_be_res_to_go in 
		let res_needed_gained = normalize (subtract_res res_to_go would_be_res_to_go) in 
		let res_gained = sum_cost res_needed_gained in 
		if (List.mem col trades_made) then default else
		if (res_gained <= num_best) then default else
		let num_to_offer_ideal = res_gained * 2 in
		let offer = get_res_to_offer our_res need num_to_offer_ideal in 
		(res_gained, Some (col, offer, res_needed_gained))
	) (0, None) plist in
	match trad with
		| None -> None
		| Some (col, give, get) ->
			(trades_made_o:=(col::trades_made)); Some (col, give, get)


(* Should we accept or deny a trade request? Procedure: If the person asking for the trade is behind us by 
	at least 2 victory points, is only asking for stuff we don't need right now, and is willing to give us 
	stuff we do need right now. Also require that the ratio of resources be at least 2:1 in our favor. *)
let handle_trade_request need our_color turn plist (inters, roads): bool = 
	let (offer_give, offer_want, their_color) = get_trade_info turn in 
	let inventory = get_res our_color plist in 
	let our_vp = get_num_vp our_color plist (inters, roads) in 
	let their_vp = get_num_vp their_color plist (inters, roads) in 
	(* If they are a threat, auto-reject their trade. We don't want to help them. *)
	if (our_vp - their_vp < 2) then false else
	let leftovers = subtract_res inventory need in 
 	let leftovers = normalize leftovers in
 	let (bl,wl,ol,gl,ll) = leftovers and (blos, wlos, olos, glos, llos) = offer_want in
 	(* If they want resources we need, reject their trade. *)
 	if (blos > bl || wlos > wl || olos > ol || glos > gl || llos > ll) then false else
 	let needed_unhad = subtract_res need inventory in
 	let needed_unhad = normalize needed_unhad in
 	let (bn,wn,on,gn,ln) = needed_unhad and (bget, wget, oget, gget, lget) = offer_give in
 	let needed_unhad = (bn > 0, wn > 0, on > 0, gn > 0, ln > 0) in 
 	let (bn, wn, on, gn, ln) = needed_unhad in 
 	(* If we don't get any resources that we need, there's no reason to make the trade. *)
 	if not ((bn && bget > 0) || (wn && wget > 0) || (on && oget > 0) || (gn && gget > 0) || (ln && lget > 0)) then false else
 	let num_get = sum_cost offer_give and num_lose = sum_cost offer_want in
 	let ratio = (num_get / num_lose) in 
 	(* If we're not at least getting double the resources we're giving up, it's not worth our time. *)
 	if (ratio < 2) then false else
 	true

(* Given the resources we have and those we need,
	try to get some of the ones we need by whatever means
	are available (sea trade, player trade, playing cards ,etc) *)
let try_for_res (hexes, ports) (inters, rlist) robberloc needo color plist goal trades_made turn:  strategy =
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
	let num_roads = get_num_roads color rlist 
	in 
		if (needo=cCOST_ROAD) && num_roads < cMAX_ROADS_PER_PLAYER 
		then	if num_roads = (cMAX_ROADS_PER_PLAYER - 1)
				then match goal with 
						| ORoad (p1,p2) -> if (valid_pair (p1, p2)) then SPlayRoadBuild ((color, (p1,p2)), None) else failwith "invalid pair in first oroad"
						| _ -> SPlayRoadBuild ((best_road inters rlist color), None) 
				else match goal with
						| ORoad (p1, p2) -> if (valid_pair (p1, p2)) then SPlayRoadBuild ((color, (p1, p2)), Some (best_road inters ((color, (p1,p2))::rlist) color)) 
					else failwith "invalid pair in second oroad"
						| _ -> NoStrategy (* should never happen! *)
		else if num_roads < cMAX_ROADS_PER_PLAYER
			then 	if num_roads = (cMAX_ROADS_PER_PLAYER - 1)
					then SPlayRoadBuild ((best_road inters rlist color), None) 
					else let first_road = best_road inters rlist color 
					in SPlayRoadBuild (first_road, Some (best_road inters (first_road::rlist) color)) 
			else NoStrategy 
		else NoStrategy
	in
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
	if (List.mem Knight cards) && ((is_robber_on_mine robberloc inters color) || (phase_is_late (inters, rlist) plist)) then
		SPlayKnight (get_robber_move hexes inters color robberloc plist needo)
	else

	(* If building a card doesn't really hurt our cause, do it. *)
	if enough_res have cCOST_CARD then
		SBuildCard else

	(* let potent_road = ref (color, (0, 0)) in

	let () = potent_road:= (if enough_res have cCOST_ROAD && get_num_roads color rlist < cMAX_ROADS_PER_PLAYER &&
		 get_num_vp color plist (inters, rlist) >= cWIN_CONDITION then match (extend_road color (inters, rlist)) with
			| None -> (color, (0, 0))
			| Some r -> r else (color, (0, 0))) in

	if (!potent_road <> (color, (0, 0))) then
		SBuildRoad (!potent_road) else *)

	(* Check for potential usefulness of player trade. *)
    let trade_strat = if (turn.tradesmade < cNUM_TRADES_PER_TURN) then
    	match (suggest_trade needo plist trades_made color (inters, rlist)) with
    		| Some (tr) -> SOfferTrade tr
    		| None -> NoStrategy
    	else NoStrategy in 
    match trade_strat with
    	| SOfferTrade tr -> SOfferTrade tr
    	| _ ->

	(* Check for usefulness of maritime trade. *)
	let ratio = calc_best_ratio ports inters color in
	let trade_res = if (b >= bn + ratio) then Some Brick 
			   else if (w >= wn + ratio) then Some Wool 
			   else if (o >= on + ratio) then Some Ore
			   else if (g >= gn + ratio) then Some Grain
			   else if (l >= ln + ratio) then Some Lumber
			   else None in
	match trade_res with
		| Some r_have -> begin match res_to_get with
			| Some r_want -> SMaritimeTrade (r_have, r_want)
			| None -> NoStrategy (* We shouldn't have called this function if we don't need a resource! *)
		 end
		| None ->

	(* We have tried everything, just give up. *)
	NoStrategy



