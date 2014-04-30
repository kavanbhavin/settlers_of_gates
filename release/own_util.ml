open Definition
open Util
(* returns hex index based on roll.*)

let piece_of_roll roll = match roll with 
| 2 -> [17]
| 3 -> [8;15]
| 4 -> [3;10]
| 5 -> [5;16]
| 6 -> [4;18]
| 8 -> [11;12]
| 9 -> [2;14]
| 10 -> [6;13]
| 11 -> [0;9]
| 12 -> [1]
| _ -> failwith "invalid roll"

(* returns a list from a cost tuple*)
let list_of_resources (b,w,o,g,l) = [b;w;o;g;l]

(* inverse of method above. 
takes a list and returns a cost tuple *)
let resources_of_list l = match l with 
| b::w::o::g::l::[] -> (b,w,o,g,l)
| _ -> failwith "incorrect type of list in resources_of_list"


 (* Uses up a certain amount of each resource. *)
 let shrink_res (b,w,o,g,l) (c,x,p,h,m) = 
	((b-c),(w-x), (o-p), (g-h), (l-m))
(* adds two cost tuples*)
let sum_of_two_costs (b,w,o,g,l) (c,x,p,h,m) = 
	((b+c),(w+x), (o+p), (g+h), (l+m))

(* Generates a random card (simulates a draw. *)
let ran_card () = 
  let choice = Random.int 5 in 
  match choice with
  | 0 -> Knight
  | 1 -> VictoryPoint
  | 2 -> RoadBuilding
  | 3 -> YearOfPlenty
  | 4 -> Monopoly
  | _ -> failwith "Random Card random out of bounds!"

  (* Returns a certain player from a player list.
  THIS FUNCTION FAILS IF THE PLAYER ISN'T IN THE LIST. *)
let get_player color plist =
  try List.find (fun (c, _, _) -> c = color) plist with
    Not_found -> failwith "get_player : Player not found!"
  
  (* Given a settlement point, return the owner's color.
   Precondition: settlement point is valid.  *)
let get_settle point inters : intersection = 
  (List.nth inters point)

  (* Given a player's color, return their resources. *)
 let get_res col plist = 
 	let (_, (res, _), _) = get_player col plist in
 	res

  (* Returns true if there exists a settlement
    < 2 road lengths away from point. *)
  let settle_one_away point inters : bool = 
    let neighbors = adjacent_points point in
    List.fold_left (fun acc v ->
      acc || (match (List.nth inters v) with
        | None -> false
        | Some _ -> true)) false neighbors

  (* Returns the number of settlements of type typ
    owned by player col. *)
  let get_num_settles col settles typ : int = 
    List.fold_left (fun acc v ->
      match v with
      | None -> acc
      | Some (settle_col, settle_type) ->
        if ((col = settle_col) && (typ = settle_type)) then
          acc+1 else acc) 0 settles

  (* Returns the number of roads owned by player col. *)
  let get_num_roads col roads : int =
    List.fold_left (fun acc (road_col, _) ->
    if (road_col = col) then acc + 1 else acc) 0 roads