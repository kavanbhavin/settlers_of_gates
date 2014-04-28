open Util
open Definition 
open Is_valid
open Build

(* Makes a minimum valid move.
   This is to be called in the case of 
   a bot giving us an invalid move. *)
let min_valid_init (map, structs, deck, discard, robber) color= 
    let (inters, roads) = structs in 
    let ipairs = List.mapi (fun i v -> (v, i)) inters in 
    let (inter, i) = try (List.find (fun (inter, i) -> begin match inter with
    | None -> begin match (get_valid_road i structs color) with
      | Some _ -> true
      | None -> false end
    | Some _ -> false
    end) ipairs) with Not_found -> failwith "No possible initial move"  in  
       match (get_valid_road i structs color) with
       | Some road -> 
         let new_town = Some (color, Town) in 
         let new_inters = List.mapi (fun i' v -> 
           if (i = i') then new_town else v) inters in
         let new_roads = road::roads in 
         (new_inters, new_roads)
       | None -> failwith "Min_valid_init_move: Move gone!"

(* Handles an initial move request. *)
let init_request ((map, structs, deck, discard, robber) : board) (p1, p2) color : structures = 
  if (free_valid_pair (p1, p2) structs) then
    let structs' = build_settlement structs p1 color Town in 
    build_road structs' (p1, p2) color
  else min_valid_init (map, structs, deck, discard, robber) color