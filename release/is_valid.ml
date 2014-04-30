open Util
open Definition

(* If p1 and p2 are adjacent. *)
let valid_pair (p1, p2) : bool = 
  p1 >= 0 && p2 >= 0 && p1 <= 53 && p2 <= 53 && 
  List.mem p2 (adjacent_points p1)

(* Is this settlement within the necessary bounds? *)
let valid_settle point = 
  point >= 0 && point <= 53  


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
  piece>=0 && piece<= 18

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


