open Util
open Definition 
open Is_valid
open Build
open Constant
open Own_util
(* Makes a minimum valid move.
   This is to be called in the case of 
   a bot giving us an invalid move. *)
let min_valid_init (map, structs, deck, discard, robber) color= 
    let (inters, roads) = structs in 
    let ipairs = List.mapi (fun i v -> (v, i)) inters in 
    (* Returns the first intersection that has an empty road
      adjacent to it. *)
    let (inter, i) = try (List.find (fun (inter, i) -> begin match inter with
    | None -> if (settle_one_away i inters) then false else
      begin match (get_valid_road i structs color) with
      | Some _ -> true
      | None -> false end
    | Some _ -> false
    end) ipairs) with Not_found -> failwith "No possible initial move"  in  
       (* Given the intersection, find the valid road that can be created,
        then build the settlement and road. *)
       match (get_valid_road i structs color) with
       | Some road -> 
         let new_town = Some (color, Town) in 
         let new_inters = List.mapi (fun i' v -> 
           if (i = i') then new_town else v) inters in
         let new_roads = road::roads in 
         (new_inters, new_roads), snd road
       | None -> failwith "Min_valid_init_move: Move gone!"

(* Handles an initial move request. *)
let init_request ((map, structs, deck, discard, robber) : board) (p1, p2) color : structures * line = 
  let (inters, roads) = structs in
  if (free_valid_pair (p1, p2) structs) && not (settle_one_away p1 inters) then
    let structs' = build_settlement structs p1 color Town in 
    (build_road structs' (p1, p2) color), (p1, p2)
  else min_valid_init (map, structs, deck, discard, robber) color

let update_init_turn num_roads color= 
      if num_roads < cNUM_PLAYERS 
      then ((next_turn color), InitialRequest) else 
      if num_roads = cNUM_PLAYERS
      then (color, InitialRequest) else 
      if num_roads < (2*cNUM_PLAYERS)
      then ((prev_turn color), InitialRequest)
    else (color, ActionRequest) 
