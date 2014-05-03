open Definition
open Constant
open Util

(* Updates the current owner of the longest road trophy.
	To be called anytime a road is built. *)
let update_longest_road plist (inters, roads) = 
	let proads = List.map (fun (pcol, phand, ptroph) -> 
		let numroads = longest_road pcol roads inters in
		((pcol, phand, ptroph), numroads)) plist in 
	let winner = List.fold_left (fun acc ((pcol, _, _), numroads) ->
		match acc with
			| None -> if (numroads >= cMIN_LONGEST_ROAD) then
				Some (pcol, numroads) else None
			| Some (pcolold, numroadsold) ->
				if (numroads > numroadsold) then
				Some (pcol, numroads) else Some (pcolold, numroadsold)) None proads in
	match winner with
		| None -> plist 
		| Some (col, _) -> List.map (fun (pcol, phand, (knights, lr, la)) ->
			if (pcol = col) then (pcol, phand, (knights, true, la)) else
			(pcol, phand, (knights, false, la))) plist

(* Updates the current owner of the largest army trophy.
	To be called anytime a knight is played. *)
let update_largest_army plist (inters, roads) = 
	let winner = List.fold_left (fun acc (pcol, _, (knights, _, _)) ->
		match acc with
			| None -> if (knights >= cMIN_LARGEST_ARMY) then
				Some (pcol, knights) else None
			| Some (pcolold, sizearmyold) ->
				if (knights > sizearmyold) then
				Some (pcol, knights) else Some (pcolold, sizearmyold)) None plist in
	match winner with
		| None -> plist 
		| Some (col, _) -> List.map (fun (pcol, phand, (knights, lr, la)) ->
			if (pcol = col) then (pcol, phand, (knights, lr, true)) else
			(pcol, phand, (knights, lr, false))) plist