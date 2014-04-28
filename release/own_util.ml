(* returns hex index based on roll. Extremel surprised this wasn't implemented.*)
let hex_of_roll roll = match roll with 
| 2 -> [17]
| 3 -> [8;15]
| 4 -> [3;10]
| 5 -> [5;16]
| 6 -> [4;18]
| 8 -> [11;12]
| 9 -> [2;14]
| 10 -> [6;13]
| 11 -> [0;9]
| 12 -> [1]
| _ -> failwith "invalid roll"

let list_of_resources (b,w,o,g,l) = [b;w;o;g;l]
let resources_of_list l = match l with 
| b::w::o::g::l::[] -> (b,w,o,g,l)
| _ -> failwith "incorrect type of list in resources_of_list"