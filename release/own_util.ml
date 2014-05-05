open Definition
open Util
open Print
open Constant
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

(* Generates a random card (simulates a draw). 
  Returns card and new deck. None if deck is empty.*)
let ran_card (deck : deck) = 
  let deck = match deck with
    | Hidden _ -> failwith "Deck is hidden!"
    | Reveal x -> x in
  let deck_shuffle = randomize deck in 
  match deck_shuffle with
    | [] -> None
    | h::t -> Some (h, Reveal t) 

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

  let get_num_vp_cards col plist = 
    let (col, (inv, cards), trophs) = get_player col plist in
    let hand = match cards with
     | Hidden _ -> failwith "Get Num VP Cards: Cards Hidden!"
     | Reveal ls -> ls in
    List.fold_left (fun acc v ->  if (v = Knight) then acc+1
      else acc) 0 hand

  let get_num_trophy_points col plist = 
    let (_, _, (_, longroad, largearmy)) = get_player col plist in
    let count = 0 in 
    let count = if (longroad) then count + cVP_LONGEST_ROAD else count in
    let count = if (largearmy) then count + cVP_LARGEST_ARMY else count in
    count

  (* Gets a player's number of victory points. *)
  let get_num_vp col plist (inters, roads) : int = 
    let town_points = cVP_TOWN * (get_num_settles col inters Town) in
    let city_points = cVP_CITY * (get_num_settles col inters City) in
    let card_points = cVP_CARD * (get_num_vp_cards col plist) in
    let troph_points = get_num_trophy_points col plist in
    town_points + city_points + card_points + troph_points

  (* Returns the winner of the game, or None if it isn't over yet. *)
  let get_winner plist (inters, roads) : color option = 
    List.fold_left (fun acc (col, hand, troph) ->
      if (get_num_vp col plist (inters, roads)) >= cWIN_CONDITION then
      (Some col) else acc) None plist

  let road_to_line_option r = 
    match r with
      | None -> None
      | Some (col, line) -> Some line

  (* Helper function for longest road that filters first. *)
  let longest_road' c roads inters = 
    let roads = List.filter (fun (col, _) -> col = c) roads in
    longest_road c roads inters

   (* Takes a resource hand and a desired res,
      removes all of that resource from the hand,
      returns the new hand and the # resources removed. *)
  let deplete_res (b,w,o,g,l) res = 
    match res with
    | Brick -> ((0, w, o, g, l), b)
    | Wool -> ((b, 0, o, g, l), w)
    | Ore ->  ((b, w, 0, g, l), o)
    | Grain -> ((b, w, o, 0, l), g)
    | Lumber -> ((b, w, o, g, 0), l)

  (* Gives num of res to the hand, returning new hand. *)
  let give_res (b,w,o,g,l) res num = 
    match res with
      | Brick -> ((b+num, w, o, g, l))
      | Wool -> ((b, w+num, o, g, l))
      | Ore ->  ((b, w, o+num, g, l))
      | Grain -> ((b, w, o, g+num, l))
      | Lumber -> ((b, w, o, g, l+num))

(** Returns a cost where there are n of the resource specified, and zero of all others *)
let mult_resource_cost (resource : resource) (n : int) : cost = 
  match resource with
    | Brick ->  (n,0,0,0,0)
    | Wool ->   (0,n,0,0,0)
    | Ore ->    (0,0,n,0,0)
    | Grain ->  (0,0,0,n,0)
    | Lumber -> (0,0,0,0,n)



let string_of_color_option color_option = 
  match color_option with 
      | Some (x) -> string_of_color x
      | None -> "No color"
