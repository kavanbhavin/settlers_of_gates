open Util
open Definition
open Constant

(* Given the number of cards we need to discard
   and the player's hand, return a minimum valid discard move *)
let min_valid_discard num (b,w,o,g,l) = 
  let temp_l = [b;w;o;g;l] in
  let ls = snd (List.fold_right (fun v (sum, acc) -> 
    if sum = num then (sum, 0::acc) else
    if (v + sum) <= num then ((v + sum), v::acc) else
      let num_discard = num - sum in 
      (num, num_discard::acc)) temp_l (0, [])) in 
  match ls with
  | b::w::o::g::l::[] -> (b, w, o, g, l)
  | _ -> failwith "Min Valid Discard fold failed!"

  (* Handles a discard request. Returns modified list of players. *)
let discard_request color plist (b,w,o,g,l) : player list = 
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
    plist'

    (* Implements the full functionality of a minimum valid discard. *)
  let min_valid_discard_full color plist : player list = 
  (* Call discard_request on bogus input to force min_valid_discard call. *)
   discard_request color plist (-1,-1,-1,-1,-1)
