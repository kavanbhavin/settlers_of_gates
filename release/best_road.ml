open Constant
open Definition
open Util
type own_point = CanBuild of int | DistanceToNearest of (int * int * int) 
let board_points = let rec helper n acc = 
	if n = (-1)
	then acc 
	else helper (n-1) (n::acc) 
in helper (cNUM_POINTS-1) [] 

let rec remove_duplicates ls = 
	let ls' = List.sort compare ls
in match ls' with 
	| a::b::tl -> if a = b then a::(remove_duplicates tl) else a::b::(remove_duplicates tl)
	| _ -> ls'

(* uses an array, that is indexed by points, to compute road distance between two points. 
The array is set to true for every point that has been seen already. *)
let distance_between_points_array p1 p2 = 
	let rec helper p acc distance = 
		if acc.(p) = true then distance
	else let lop = ref [] in 
	for i=0 to ((Array.length acc)-1) do 
	if acc.(i) = true 
	then lop := (adjacent_points i) @ !lop
else ()
done; (List.iter (fun element -> acc.(element) <- true) !lop); 
helper p acc (distance+1)
in let init_array = Array.make cNUM_POINTS false in (init_array.(p2) <- true); 
helper p1 init_array 0  


(* extremely slow (but functional) implementation of distance between points function *)
let distance_between_points p1 p2 =
	let rec helper p lop distance = 
		if List.mem p lop 
		then distance
		else helper p (remove_duplicates (List.flatten (List.map adjacent_points lop))) (distance+1)
	in helper p1 [p2] 1  


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
    let neighbors = adjacent_points point in
    List.fold_left (fun acc v ->
      acc || (match (List.nth inters v) with
        | None -> false
        | Some _ -> true)) false neighbors

let should_build_road point inters = let point_is_empty = match (List.nth inters point) with 
				| None -> true 
				| _ -> false 
			in point_is_empty && (not(settle_one_away point inters))

let convert_to_our_type inters= 
	List.map (fun i-> if should_build_road i inters then CanBuild i else DistanceToNearest (i,0,0) ) board_points

let only_free_and_buildable_points inters : own_point list = 
	List.filter (fun a -> 
	match a with 
	| CanBuild i -> 
		begin match List.nth inters i with 
				| None -> true
				| _ -> false
			end
	| _ -> false ) (convert_to_our_type inters)

let fill_in_distances inters : own_point list = 
	List.rev (List.fold_left (fun (acc : own_point list) (ele : own_point) -> 
	match ele with 
	| DistanceToNearest (i, _, _) -> let distances : int list = 
			List.map (fun a -> 
				begin match a with 
					| CanBuild (a) -> (memoized_distance (a,i)) 
					| _ -> failwith "not can build in free and buildable?"
				end
	) (only_free_and_buildable_points inters)
		in let (min_distance, second_min_distance) : int * int = (List.fold_left (fun (min1, min2) element -> 
			if element < min1 
			then (element, min1)
			else if element < min2
			then (min1, min2)
else (min1, min2)) (cNUM_POINTS, cNUM_POINTS) distances)
in ((DistanceToNearest (i, min_distance, second_min_distance))::acc)
	| _ -> ele::acc) [] (convert_to_our_type inters))

let sorted_distances inters= let fid = fill_in_distances inters 
in let check_order = let bools = List.mapi (fun i a -> match a with 
					| CanBuild j 
					| DistanceToNearest (j, _, _) -> i=j ) fid
in List.fold_left (fun acc ele -> acc && ele) true bools in if check_order then (Printf.printf "order was correct"; fid) 
else failwith "incorrect order" (*((Printf.printf "order was incorrect"); List.sort (fun a b -> 
	begin match (a,b) with 
		| (CanBuild i, CanBuild j) 
		| (CanBuild i, DistanceToNearest (j, _, _ )) 
		| ((DistanceToNearest (i, _, _), CanBuild j))
		| ((DistanceToNearest (i, _, _), DistanceToNearest(j, _, _))) -> compare i j
	end ) fid)
*)

(*Given a point, returns the best possible other point to build a road too.*)
let best_road_from_point p inters rlist: int = let already_built_roads_from_point = 
	List.fold_left (fun acc (_, (p1,p2)) ->
	 if p1=p then p2::acc else if p2=p then p1::acc else acc) [] rlist 
in let s = (sorted_distances inters) 
in let our_type = List.map (fun a -> List.nth s a) (adjacent_points p) 
in let our_type_with_built_roads_removed = List.filter (fun a -> match a with 
			| CanBuild i 
			| DistanceToNearest (i, _ , _ ) -> not(List.mem i already_built_roads_from_point)) our_type
	in let asd = List.fold_left (fun (acc : own_point) (ele : own_point) -> 
	match (acc, ele) with 
		| (CanBuild _, CanBuild _) -> acc  
		| (CanBuild _, DistanceToNearest (_, _, _ )) -> acc
		| ((DistanceToNearest (_, _, _), CanBuild _)) -> ele 
		| ((DistanceToNearest (_, d1, d2), DistanceToNearest(_, d3, d4))) -> if (d1+d2) <= (d3+d4) then acc else ele) (DistanceToNearest (1, cNUM_POINTS, cNUM_POINTS)) our_type_with_built_roads_removed
in match asd with 
| CanBuild i -> i 
| DistanceToNearest (j, _, _) -> j
