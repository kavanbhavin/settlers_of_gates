open Util
open Definition
open Constant
open Own_util
open Robber
(*make sure the person has enough cards before discard*)

 (* Randomly tries to discard cards until it has enough.
  Will probably loop forever if the player doesn't actually have
  enough resources, but this probably hopefully shouldn't happen. *)
 let rec min_valid_discard' num (b,w,o,g,l) (bd,wd,od,gd,ld) curr : cost = 
  let discarded = (bd,wd,od,gd,ld) in
  if (curr = num) then discarded else
  let el = match (pick_random [Brick;Wool;Ore;Grain;Lumber]) with
    | None -> failwith "impossible"
    | Some x -> x in
  match el with
    | Brick -> if b = 0 then min_valid_discard' num (b,w,o,g,l) discarded curr else
      min_valid_discard' num (b, w, o, g, l) (bd+1, wd, od, gd, ld) (curr+1)
    | Wool -> if w = 0 then min_valid_discard' num (b,w,o,g,l) discarded curr else
      min_valid_discard' num (b, w-1, o, g, l) (bd, wd+1, od, gd, ld) (curr+1)
    | Ore -> if o = 0 then min_valid_discard' num (b,w,o,g,l) discarded curr else
      min_valid_discard' num (b, w, o-1, g, l) (bd, wd, od+1, gd, ld) (curr+1)
    | Grain -> if g = 0 then min_valid_discard' num (b,w,o,g,l) discarded curr else
      min_valid_discard' num (b, w, o, g-1, l) (bd, wd, od, gd+1, ld) (curr+1)
    | Lumber -> if l = 0 then min_valid_discard' num (b,w,o,g,l) discarded curr else
      min_valid_discard' num (b, w, o, g, l-1) (bd, wd, od, gd, ld+1) (curr+1)

(* Given the number of cards we need to discard
   and the player's hand, return a minimum valid discard move *)
let min_valid_discard num inventory = 
  (* let ls = snd (List.fold_right (fun v (sum, acc) -> 
    if sum = num then (sum, 0::acc) else
    if (v + sum) <= num then ((v + sum), v::acc) else
      let num_discard = num - sum in 
      (num, num_discard::acc)) (list_of_resources inventory) (0, [])) in 
  resources_of_list ls *)
  let discarded = min_valid_discard' num inventory (0, 0, 0, 0, 0) 0 in
  discarded

let need_to_discard color plist : bool = 
  let (color, ((bh, wh, oh, gh, lh), cards), trophies) = try List.find (fun (c, _, _) ->
    (c = color)) plist with Not_found ->
    failwith "Discard Request: Player doesn't exist!" in
  let total_cards = bh + wh + oh + gh + lh in 
  total_cards > cMAX_HAND_SIZE

(* Returns the next player to discard, or None. *)
let rec get_next_discard_player my_color active_color plist : color option =
  if (my_color = active_color) then 
    (if (need_to_discard my_color plist) then Some my_color else
    None)
  else if (need_to_discard my_color plist) then Some my_color else
    get_next_discard_player (next_turn my_color) active_color plist

  (* Handles a discard request. Returns modified list of players. *)
let discard_request color plist (b,w,o,g,l) active_player : (next * player list * cost) = 
  let (color, ((bh, wh, oh, gh, lh), cards), trophies) = try List.find (fun (c, _, _) ->
    (c = color)) plist with Not_found -> 
    failwith "Discard Request: Player doesn't exist!" in
  let total_cards = bh + wh + oh + gh + lh in 
  if (total_cards <= cMAX_HAND_SIZE) then failwith
    "Asked to discard cards when didn't have over cMAX_HAND_SIZE!"
  else 
    let discard_num = (total_cards/2) in 
    let num_given = b + w + o + g + l in 
    let good_amounts = b <= bh && w <= wh && o <= oh && g <= gh && l <= lh &&
    b >= 0 && w >= 0 && o >=0 && g >= 0 && l >= 0 in
    let (b,w,o,g,l) = if (num_given < discard_num) || not (good_amounts) then
      (min_valid_discard discard_num (bh,wh,oh,gh,lh))
      else (b, w, o, g, l) in 
    let new_hand = (bh-b, wh-w, oh-o, gh-g, lh-l) in 
    let plist' = List.map (fun (c, hand, trophies) ->
      if (c = color) then (c, (new_hand, cards), trophies) else 
        (c, hand, trophies)) plist in 
    if active_player= color then ((active_player, RobberRequest), plist', (b,w,o,g,l))
  else match get_next_discard_player (next_turn color) active_player plist' with
    | Some next_color -> ((next_color, DiscardRequest), plist', (b,w,o,g,l))
    | None -> ((active_player, RobberRequest), plist', (b,w,o,g,l))

    (* Implements the full functionality of a minimum valid discard. *)
  let min_valid_discard_full color plist active_player : (next * player list * cost) = 
  (* Call discard_request on bogus input to force min_valid_discard call. *)
   discard_request color plist (-1,-1,-1,-1,-1) active_player
