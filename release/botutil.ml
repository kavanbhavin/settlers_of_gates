open Util
open Definition
open Constant


  type objective = NA | OTown of int | OCity of int | OKnight | ORoad of int * int
  type to_buy = RoadBuy | TownBuy | CityBuy | CardBuy

(* Returns a certain player from a player list.
THIS FUNCTION FAILS IF THE PLAYER ISN'T IN THE LIST. *)
let get_player color plist =
	try List.find (fun (c, _, _) -> c = color) plist with
	Not_found -> failwith "get_player : Player not found!"

let get_res color plist = 
	let (_, (res, _), _) = get_player color plist in res

let enough_res (b,w,o,g,l) (bn, wn, on, gn, ln) : bool =
	b >= bn && w >= wn && o >= on && g >= gn && l >= ln

let can_afford want color plist = 
	let have = get_res color plist in
	match want with
	| RoadBuy -> enough_res have cCOST_ROAD
	| TownBuy -> enough_res have cCOST_TOWN
	| CityBuy -> enough_res have cCOST_CITY
	| CardBuy -> enough_res have cCOST_CARD

(* Expected number of times a number is rolled 
	out of 36 rolls, scaled as necessary. *)
let expected_of_36 num = match num with
	| 2 | 12 -> 1
	| 3 | 11 -> 3
	| 4 | 10 -> 6
	| 5 | 9 -> 10
	| 6 | 8 -> 15
	| 7 -> 20
	| _ -> 0

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
let weight_loc settle num board res_owned= 
	let ((hexlist, _), _, _, _, _) = board in 
	match settle with
		| Some _ -> 0
		| None ->
	let hexes_gained = adjacent_pieces num in
	fst (List.fold_left (fun (acc, curlist) hex_gained ->
		try (let (terrain,roll) = List.nth hexlist hex_gained in
		let (acc', div_factor, res) =
		 let res_owned' = curlist@res_owned in
		 match (resource_of_terrain terrain) with
			| None -> (0, 1, [])
			| Some Grain -> (30, 1 + (num_owned Grain res_owned') * 4, [Grain])
			| Some Lumber -> (100, 1 + (num_owned Lumber res_owned') * 4, [Lumber])
			| Some Brick -> (90, 1 + (num_owned Brick res_owned') * 4, [Brick])
			| Some Wool -> (30, 1 + (num_owned Wool res_owned') * 4, [Wool])
			| Some Ore -> (10, 1 + (num_owned Ore res_owned') * 4, [Ore]) in
		let acc' = acc' * (expected_of_36 roll) / div_factor in
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
      (inter, n, (weight_loc inter n board res_tiles))) inters in
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

(* Returns true if there exists a settlement
< 2 road lengths away from point. *)
let settle_one_away point inters : bool = 
let neighbors = adjacent_points point in
List.fold_left (fun acc v ->
  acc || (match (List.nth inters v) with
    | None -> false
    | Some _ -> true)) false neighbors

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
	let weighted_locs = sort_locs inters board res_tiles in
	let nearby_options = get_near_empty (inters, roads) color false in
	let nearby_options = if (List.length nearby_options) = 0 then 
		get_near_empty (inters, roads) color false else nearby_options in
	let rec choose_move wl : objective = 
		match wl with
		| [] -> NA
		| (_, h, _)::t -> 
			if (h <> loc) then choose_move t else
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