open Definition
open Util

let end_turn (turn : turn) (plist: player list) : turn * (player list)=
	let cards_to_be_added = reveal turn.cardsbought 
in let plist' = List.map (fun (color, (inventory, cards), trophies) -> 
	if color = turn.active then
let new_cards = List.fold_left append_card cards cards_to_be_added 
	in (color, (inventory, new_cards), trophies)
else (color, (inventory, cards), trophies)) plist
in let turn' = new_turn (next_turn turn.active)
in (turn', plist')