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

