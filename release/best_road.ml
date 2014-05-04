open Constant
open Definition
open Util
type own_point = CanBuild of int | DistanceToNearest of (int * int * int) 
let board_points = let rec helper n acc max = 
	if n = max
	then acc 
	else helper (n+1) (n::acc) max
in helper 0 [] cNUM_POINTS

let inters : (intersection list) ref = ref []

let init_best_road inters'=
	inters:= inters'


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
	for i=0 to Array.length acc do 
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
  let settle_one_away point : bool = 
    let neighbors = adjacent_points point in
    List.fold_left (fun acc v ->
      acc || (match (List.nth !inters v) with
        | None -> false
        | Some _ -> true)) false neighbors

let convert_to_our_type = 
	let a = List.map (fun i-> (i, settle_one_away i)) board_points
	in List.map (fun (i, b) -> if b then DistanceToNearest (i,0,0) else CanBuild i) a 

let only_free_and_buildable_points : own_point list = 
	List.filter (fun a -> 
	match a with 
	| CanBuild i -> 
		begin match List.nth !inters i with 
				| None -> true
				| _ -> false
			end
	| _ -> false ) convert_to_our_type

let fill_in_distances : own_point list = 
	List.rev (List.fold_left (fun (acc : own_point list) (ele : own_point) -> 
	match ele with 
	| DistanceToNearest (i, _, _) -> let distances : int list = 
			List.map (fun a -> 
				begin match a with 
					| CanBuild (a) -> (distance_between_points_array a i) 
					| _ -> failwith "not can build in free and buildable?"
				end
	) only_free_and_buildable_points 
		in let (min_distance, second_min_distance) : int * int = (List.fold_left (fun (min1, min2) element -> 
			if element < min1 
			then (element, min1)
			else if element < min2
			then (min1, min2)
else (min1, min2)) (cNUM_POINTS, cNUM_POINTS) distances)
in ((DistanceToNearest (i, min_distance, second_min_distance))::acc)
	| _ -> ele::acc) [] only_free_and_buildable_points)

let sorted_distances = List.sort (fun a b -> 
	begin match (a,b) with 
		| (CanBuild i, CanBuild j) 
		| (CanBuild i, DistanceToNearest (j, _, _ )) 
		| ((DistanceToNearest (i, _, _), CanBuild j))
		| ((DistanceToNearest (i, _, _), DistanceToNearest(j, _, _))) -> compare i j
	end ) fill_in_distances

let best_road_from_point p : int= let our_type = List.map (fun a -> List.nth sorted_distances a) (adjacent_points p) 
	in let asd = List.fold_left (fun (acc : own_point) (ele : own_point) -> 
	match (acc, ele) with 
		| (CanBuild _, CanBuild _) -> acc  
		| (CanBuild _, DistanceToNearest (_, _, _ )) -> acc
		| ((DistanceToNearest (_, _, _), CanBuild _)) -> ele 
		| ((DistanceToNearest (_, d1, d2), DistanceToNearest(_, d3, d4))) -> if (d1+d2) <= (d3+d4) then acc else ele) (DistanceToNearest (1, cNUM_POINTS, cNUM_POINTS)) our_type
in match asd with 
| CanBuild i -> i 
| DistanceToNearest (j, _, _) -> j
