open Definition
open Util
open Is_valid
open Own_util


let find_random_piece () = Random.int 19
let rec min_valid_robber turn board plist = do_robber_move turn.active (find_random_piece (), None) board plist 
and do_robber_move active_player ((piece: piece), color) (map, structs, deck, discard, robber) (plist: player list) : (robber * player list) =
	let index_to_remove = ref (-1)
in if (is_valid_piece piece) then let robber' = piece in begin match color with  
| None -> (robber', plist)
| Some color_to_rob -> let plist' = List.map (fun (color', (inventory, cards), trophies) ->if color'= color_to_rob
	then let resources= list_of_resources inventory
in let indices= randomize [0;1;2;3;4] 
in index_to_remove := (try List.find (fun index -> (List.nth resources index)>0) indices with _ -> failwith "no resource to subtract");
let inventory'=List.mapi (fun index element-> if index = (!index_to_remove) 
	then element-1 
else element
 ) resources
in (color', ((resources_of_list inventory'), cards), trophies)
else (color_to_rob, (inventory, cards), trophies)) plist
in (robber', (List.map (fun (color, (inventory, cards), trophies)-> if color = active_player 
	then let inventory'= let resources = list_of_resources inventory in 
	resources_of_list (List.mapi (fun index element-> if index= (!index_to_remove) then element+1 else element) resources)
	in (color, (inventory', cards), trophies) else (color, (inventory, cards), trophies) ) plist'))
end 
else failwith "invalid piece"