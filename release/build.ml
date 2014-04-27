open Definition
open Is_valid

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