open Definition
open Is_valid
open Constant

(* BUILD STUFF *)

let build_settlement (inters, rlist) p1 color settle_type : structures = 
  if not (empty_settlement p1 inters) then 
    failwith "Trying to override settlement." 
  else 
    ((List.mapi (fun i el -> if (i = p1) then 
        Some (color, settle_type) else el) inters), rlist)

let build_road (inters, rlist) line color : structures = 
  if (not (empty_road line rlist)) then 
    failwith "Trying to override road"
  else 
    (inters, ((color, line)::rlist))

 (* Given a player's color, return their resources. *)
 let get_res col plist = 
 	let (_, (res, _), _) = get_player col plist in
 	res

 (* Uses up a certain amount of each resource. *)
 let shrink_res a b = 
 	let (b,w,o,g,l) = a and (x,x1,x2,x3,x4) = b in
 	(b-x, w-x1, o- x2, g - x3, l - x4)

 (* Checks if we can build a road (enough resources and its free. *)
 let can_build_road (inters, rlist) line col plist = 
 	let (b,w,o,g,l) = get_res col plist in
 	let (bn, wn, on, gn, ln) = cCOST_ROAD in 
	let enough_res = b >= bn && w >= wn && o >= on && g >= gn && l >= ln in
	let is_free = empty_road line rlist in 
	enough_res && is_free

(* Goes through the process of building a road, returning the (structs, plist) pair
	where structs is the updated structures and plist is the player list
	with resources updated for the proper player. Precondition: Player can afford road. *)
let build_road_move (inters, rlist) line color plist = 
	let plist' = List.map (fun (col, (res, hand), troph) ->
		if (col = color) then (col, ((shrink_res res cCOST_ROAD), hand), troph)
		else (col, (res, hand), troph)) plist in
	let structs' = build_road (inters, rlist) line color in
	(structs', plist')