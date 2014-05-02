open Util
open Definition
open Constant
open Own_util
open Robber
(*make sure the person has enough cards before discard*)


(* Given the number of cards we need to discard
   and the player's hand, return a minimum valid discard move *)
let min_valid_discard num inventory = 
  let ls = snd (List.fold_right (fun v (sum, acc) -> 
    if sum = num then (sum, 0::acc) else
    if (v + sum) <= num then ((v + sum), v::acc) else
      let num_discard = num - sum in 
      (num, num_discard::acc)) (list_of_resources inventory) (0, [])) in 
  resources_of_list ls

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
let discard_request color plist (b,w,o,g,l) active_player : (next * player list) = 
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
    if active_player= color then ((active_player, RobberRequest), plist')
  else match get_next_discard_player (next_turn color) active_player plist' with
    | Some next_color -> ((next_color, DiscardRequest), plist')
    | None -> ((active_player, RobberRequest), plist')

    (* Implements the full functionality of a minimum valid discard. *)
  let min_valid_discard_full color plist active_player : (next * player list) = 
  (* Call discard_request on bogus input to force min_valid_discard call. *)
   discard_request color plist (-1,-1,-1,-1,-1) active_player
