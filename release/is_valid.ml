open Util
open Definition

(* CHECK VALID INITIAL MOVES *)

(* If p1 and p2 are adjacent. *)
let valid_pair (p1, p2) : bool = 
  p1 >= 0 && p2 >= 0 && p1 <= 53 && p2 <= 53 && 
  List.mem p2 (adjacent_points p1)

let empty_road (p1, p2) roads : bool = 
  not (List.exists (fun (_, line) ->
    (line = (p1, p2) || line = (p2, p1))) roads)

(* PRECONDITION: p1 is a valid indices. *)
let empty_settlement p1 intersections : bool =
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
  let corner_pairs = List.map (fun v -> (settle, [v])) (adjacent_points settle) in
  let good_pair = 
    try Some (List.find (fun (start_loc, adj) -> try ignore (List.find (fun end_loc ->
     free_valid_pair (start_loc, end_loc) structs) adj); true
     with Not_found -> false) corner_pairs) with Not_found -> None in
  match good_pair with
  | Some (start_loc, adj) ->
    let end_loc = try (List.find (fun end_loc ->
     free_valid_pair (start_loc, end_loc) structs) adj) with
        Not_found -> failwith "Get_Valid_Road failed to remove invalid start points!" in
    Some (color, (start_loc, end_loc))
  | None -> None

  