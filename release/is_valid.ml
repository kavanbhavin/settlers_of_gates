open Util
open Definition
open Own_util
open Constant

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

(* Returns "player col owns a road with one point
    on point." *)
let own_road_with_point point col roads : bool = 
  List.fold_left (fun acc (r_c, (r_s, r_e)) -> 
    acc || ((r_s = point || r_e = point) && r_c = col)) false roads

(* Returns "player col owns a settlement on point.
  Precondition: Point is valid." *)
let own_settle_with_point point col inters : bool = 
  if not (valid_settle point) then false else
  match (List.nth inters point) with
    | None -> false
    | Some (settle_col, _) -> (col = settle_col)

(* Returns true if road is adjacent to a road or settlement owned by color. *)
let road_in_range col (inters, roads) (start_loc, end_loc) = 
    let near_road = start_loc::[end_loc] in
    let own_adj_road = List.fold_left (fun acc point ->
      acc || (own_road_with_point point col roads)) false near_road in
    own_adj_road 

(* Returns true if the road is a adjacent to a road owned by color,
  and is far enough away from existing settlements to be built. *)
let settle_in_range col (inters, roads) settle : bool = 
  let own_adj_road = own_road_with_point settle col roads in
  let too_close = settle_one_away settle inters in
  own_adj_road && (not too_close)

(* Returns true if r1 is free and can be built by col.
  Assumes that building a road requires no resources. *)
let can_build_road_free r1 col (inters, roads) = 
  let is_free = empty_road r1 roads in 
  let in_range = road_in_range col (inters, roads) r1 in
  let not_too_many = (get_num_roads col roads) < cMAX_ROADS_PER_PLAYER in 
  is_free && in_range && not_too_many

(* Returns true if r1 is free and can be built by col,
  and if r2 is either None or can also be built. *)
let can_build_roads_free r1 r2_o col structs = 
    (can_build_road_free r1 col structs) &&
      (match r2_o with
        | None -> true
        | Some r2 -> (can_build_road_free r2 col structs))

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
