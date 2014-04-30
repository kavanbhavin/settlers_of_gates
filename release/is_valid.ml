open Util
open Definition
open Own_util
open Constant

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
    let near_start = adjacent_points start_loc and
        near_end   = adjacent_points end_loc in
    let near_road = near_start@near_end in
    let own_adj_road = List.fold_left (fun acc point ->
      acc || (own_road_with_point point col roads)) false near_road in
    let own_adj_settle = List.fold_left (fun acc point ->
      acc || (own_settle_with_point point col inters)) false near_road in
    own_adj_road || own_adj_settle

(* Returns true if the road is a adjacent to a road owned by color,
  and is far enough away from existing settlements to be built. *)
let settle_in_range col (inters, roads) settle : bool = 
  let neighbors = adjacent_points settle in
  let own_adj_road = List.fold_left (fun acc point ->
    acc || (own_road_with_point point col roads)) false neighbors in
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
let can_build_roads_free r1 r2 col structs plist = 
    failwith "unimplemented"


