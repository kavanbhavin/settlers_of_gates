open Definition
open Util
open Is_valid

let list_of_resources (b,w,o,g,l) = [b;w;o;g;l]
let resources_of_list l = match l with 
| b::w::o::g::l::[] -> (b,w,o,g,l)
| _ -> failwith "incorrect type of list in resources_of_list"

let do_robber_move active_player ((piece: piece), color) (map, structs, deck, discard, robber) (plist: player list) : (robber * player list) =
if (is_valid_piece piece) then let robber' = piece in begin match color with  
| None -> (robber', plist)
| Some color_to_rob -> let plist' = List.map (fun (color', (inventory, cards), trophies) ->if color'= color_to_rob
	then let resources= list_of_resources inventory
in let indices= randomize [0;1;2;3;4] 
in let index_to_remove = List.find (fun index -> (List.nth resources index)>0) indices
in let inventory'=List.mapi (fun index element-> if index=index_to_remove 
	then element-1 
else element
 ) resources
in (color', ((resources_of_list inventory'), cards), trophies)
else (color_to_rob, (inventory, cards), trophies)) plist
in (robber', plist')
end 
else failwith "invalid piece"