open Definition
open Constant
open Util
open Print


type game = state

let state_of_game g = g
let game_of_state s = s


let init_game () = game_of_state (gen_initial_state())

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

(* BUILD STUFF *)

let build_settlement (inters, rlist) p1 color settle_type : structures = 
  if not (empty_settlement p1 inters) then 
    failwith "Trying to override settlement." 
  else 
    ((List.mapi (fun i el -> if (i = p1) then 
        Some (color, settle_type) else el) inters), rlist)

let build_road (inters, rlist) line color : structures = 
  if (not (empty_road line rlist)) then 
    failwith "Trying to override road"
  else 
    (inters, ((color, line)::rlist))
  

let min_valid board move = 
  failwith "not implemented"

let init_request ((map, structs, deck, discard, robber) : board) (p1, p2) color : structures = 
  if (free_valid_pair (p1, p2) structs) then
    let structs' = build_settlement structs p1 color Town in 
    build_road structs' (p1, p2) color
  else min_valid (map, structs, deck, discard, robber)  (InitialMove (p1, p2))

let handle_move ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) m =
  let board = (map, structs, deck, discard, robber) in 
  match m with
  | InitialMove l -> begin
    match req with
    | InitialRequest ->
      let structs' = (init_request board l color) in 
      (None, ((map, structs', deck, discard, robber), plist, turn, (color, req)))
    | _ -> min_valid board m
  end
  | _ -> failwith "unimplemented"

let presentation s = failwith "Were not too much to pay for birth."
