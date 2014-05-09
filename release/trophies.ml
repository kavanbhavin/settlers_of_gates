open Definition
open Constant
open Util
open Own_util

(* Sets col to have the longest road. *)
let set_longest_road col plist = 
	List.map (fun (pcol, phand, (knights, lr, la)) ->
			if (pcol = col) then (pcol, phand, (knights, true, la)) else
			(pcol, phand, (knights, false, la))) plist

(* Sets col to have the largest army. *)
let set_largest_army col plist = 
	List.map (fun (pcol, phand, (knights, lr, la)) ->
			if (pcol = col) then (pcol, phand, (knights, lr, true)) else
			(pcol, phand, (knights, lr, false))) plist

(* Updates the current owner of the longest road trophy.
	To be called anytime a road is built. *)
let update_longest_road plist (inters, roads) col : player list= 
	let curr_holder = List.fold_left (fun acc (col, h, (k, lr, la)) ->
		if lr then Some (col, h, (k, lr, la)) else acc) None plist in
	match curr_holder with
		| Some (pcol, _, _) ->
			let longest_road = longest_road' pcol roads inters in 
			let our_guys_road = longest_road' col roads inters in 
			if (our_guys_road > longest_road) then set_longest_road col plist else
			plist
		| None ->
			let our_guys_road = longest_road' col roads inters in 
			if (our_guys_road >= cMIN_LONGEST_ROAD) then set_longest_road col plist else
			plist

(* Updates the current owner of the largest army trophy.
	To be called anytime a knight is played. *)
let update_largest_army plist (inters, roads) color = 
	let curr_holder = List.fold_left (fun acc (col, h, (k, lr, la)) ->
		if la then Some (col, h, (k, lr, la)) else acc) None plist in
	match curr_holder with
		| Some (pcol, _, (k, _, _)) ->
			let largest_army = k in 
			let (_, _, (our_knights, _, _)) = get_player color plist in
			let our_guys_army = our_knights in 
			if (our_guys_army > largest_army) then set_largest_army color plist else
			plist
		| None ->
			let (_, _, (our_knights, _, _)) = get_player color plist in
			let our_guys_army = our_knights in 
			if (our_guys_army >= cMIN_LARGEST_ARMY) then set_largest_army color plist else
			plist