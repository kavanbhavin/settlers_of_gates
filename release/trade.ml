open Is_valid
open Definition
open Own_util
open Constant
open Util

(* Handles a response to a trade. Returns the new player list,
	with resources exchanged if necessary. ASSUMES THE TRADE IS VALID. *)
let handle_trade accept origin (color, getting, giving) plist =
	(* Located player info for both players involved in trade. *)
	if (not accept) then plist else  
	let (origin_color, origin_hand, origin_trophies) =
		get_player origin plist in 
	let ((b1, w1, o1, g1, l1), origin_cards) = origin_hand in 
	let (our_color, our_hand, our_trophies) =
		get_player color plist in
	let ((b2, w2, o2, g2, l2), our_cards) = our_hand in 
	(* Getting is the resources we get, losing is those we lose. *)
	let (bg, wg, og, gg, lg) = getting in
	let (bl, wl, ol, gl, ll) = giving in
	(* Compute new hands for both them and us. *)
	let origin_hand' = ((b1-bg+bl, w1-wg+wl, o1-og+ol, 
		g1-gg+gl, l1-lg+ll), origin_cards) in 
	let our_hand' = ((b2+bg-bl, w2+wg-wl, o2+og-ol,
		g2+gg-gl, l2+lg-ll), our_cards) in
	List.map (fun (col, hand, troph) -> if (col = origin) then
		(col, origin_hand', troph) else if (col = color) then
		(col, our_hand', troph) else (col, hand, troph)) plist

(* Extracts a (color, trade) data pair from a turn.
	Fails if no trade is pending. *)
let get_trade_info turn : (color * trade) = 
	let origin = turn.active in
	let trade = (match turn.pendingtrade with 
	    | Some tr -> tr
	    | None -> failwith "Trade Request made with no pending trade!") in
	(origin, trade)

(* Updates a turn after a trade is made, resetting the 
	pending trade and incrementing the # of trades made. *)
let update_turn_after_trade turn : turn = 
	{active=turn.active;dicerolled=turn.dicerolled;cardplayed=turn.cardplayed;
	cardsbought=turn.cardsbought; tradesmade=turn.tradesmade+1; pendingtrade = None} 

(* Given a trade and a player list, determine whether or 
	not that trade is valid. *)
let valid_trade (their_color, c1, c2) my_color plist= 
	let (_, (their_stuff, _), _) = get_player their_color plist in 
	let (_, (my_stuff, _), _) = get_player my_color plist in
	let (br1, wr1, or1, gr1, lr1) = c1 in
	let (br2, wr2, or2, gr2, lr2) = c2 in 
	let (bh1, wh1, oh1, gh1, lh1) = my_stuff in
	let (bh2, wh2, oh2, gh2, lh2) = their_stuff in
	br1 >= 0 && wr1 >= 0 && or1 >= 0 && gr1 >= 0 && lr1 >= 0 &&
	br2 >= 0 && wr2 >= 0 && or2 >= 0 && gr2 >= 0 && lr2 >= 0 &&
	br1 <= bh1 && wr1 <= wh1 && or1 <= oh1 && gr1 <= gh1 && lr1 <= lh1 &&
	br2 <= bh2 && wr2 <= wh2 && or2 <= oh2 && gr2 <= gh2 && lr2 <= lh2

(* We want to request a trade: Make sure it is valid, and return (turn, next) with
	turn including info on the trade and next prompting the other player to respond. *)
let update_turn_before_trade turn next trade my_color plist : turn * next= 
	if not (valid_trade trade my_color plist) then failwith "Invalid trade not caught!" else
	let turn' =  {active=turn.active;dicerolled=turn.dicerolled;cardplayed=turn.cardplayed;
	cardsbought=turn.cardsbought; tradesmade=turn.tradesmade; pendingtrade = Some trade} in
	let (trade_with, _, _) = trade in
	let next' = (trade_with, TradeRequest) in
	(turn', next')

(* Calculate the best ratio the player can
	use to trade in the given resource. Must be either
	2, 3, or 4. *)
let calc_best_ratio (ports : port list) inters col res : ratio = 
	List.fold_left (fun best_ratio (line, ratio, port_res) ->
		let right_type = match port_res with
			| Any -> true 
			| PortResource port_res -> (port_res = res) in
		if (owns_port line col inters) && ratio < best_ratio && right_type then
		ratio else best_ratio
	) cMARITIME_DEFAULT_RATIO ports

(* If the player can make the sea trade specified, 
	return Some of the new playerlist. Otherwise
	return None. *)
let make_sea_trade map structs col (sold, bought) plist = 
	let (inters, roads) = structs in
	let (hex, ports) = map in 
	let best_ratio = calc_best_ratio ports inters col sold in
	let (pcol, (pinv, pcards), ptroph) = get_player col plist in 
	let player_has = num_resource_in_inventory pinv sold in
	if (player_has < best_ratio) then None else
	let those_lost = mult_resource_cost sold best_ratio in
	let those_gained = single_resource_cost bought in
	let delta_res = shrink_res those_gained those_lost in
	let res' = sum_of_two_costs pinv delta_res in
	let plist' = List.map (fun (pc, (pi, pca), pt) ->
		if (pc = col) then (pc, (res', pca), pt) else
		(pc, (pi, pca), pt)) plist in
	Some plist'