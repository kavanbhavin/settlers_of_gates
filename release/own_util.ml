(* returns hex index based on roll.*)
let piece_of_roll roll = match roll with 
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

(* returns a list from a cost tuple*)
let list_of_resources (b,w,o,g,l) = [b;w;o;g;l]

(* inverse of method above. 
takes a list and returns a cost tuple *)
let resources_of_list l = match l with 
| b::w::o::g::l::[] -> (b,w,o,g,l)
| _ -> failwith "incorrect type of list in resources_of_list"

(* adds two cost tuples*)
let sum_of_two_costs (b,w,o,g,l) (c,x,p,h,m) = 
	((b+c),(w+x), (o+p), (g+h), (l+m))

