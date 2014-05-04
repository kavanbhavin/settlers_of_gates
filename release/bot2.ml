open Definition
open Registry
open Constant
open Util
open Botutil

(** Give your bot a 2-20 character name. *)
let name = "hibot"


module Bot = functor (S : Soul) -> struct

  (* A pointer to the current objective. *)
  let goal = ref NA

  (* A running list of which resources we own generator tiles for. *)
  let (res_tiles : resource list ref) = ref []

  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = 
    (res_tiles := []);
     goal:=NA

  (* Uses weightings from botutil to pick an initial move. *)
  let choose_init_move (inters, roads) board color : line = 
    let weighted_inters = sort_locs inters board !res_tiles in
    let rec pick_move wi : line = 
      match wi with
      | [] -> (0, 0)
      | (_, h, score)::t -> match get_valid_road h (inters, roads) color with
        | None -> pick_move t
        | Some _ ->
          let () = print_endline ("score: "^(string_of_int score)) in
          (add_res h res_tiles board); 
            get_init_road (inters, roads) !res_tiles board color h in
    pick_move weighted_inters

   (* We have yet to build the maximum number of towns.
    Work towards our next town!. *)
   let town_builder (inters, roads) board color plist : unit = 
    let (_, ((b,w,o,g,l), _), _) = get_player color plist in
    (goal:= town_move (inters, roads) !res_tiles board (b,w,o,g,l) color)

   (* Default action move: either roll dice or end turn. *)
   let default_action turn = 
    if is_none turn.dicerolled then Action(RollDice) else Action(EndTurn)

  (* Invalid moves are overridden in game *)
  let handle_request ((board, plist, turn, next) : state) : move =
    let (color, request) = next in
    let (map, structs, deck, discard, robber) = board in 
    let (inters, roads) = structs in
    match request with
      | InitialRequest -> InitialMove (choose_init_move structs board color) 
      | RobberRequest -> RobberMove(0, None)
      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeResponse(false)
      | ActionRequest -> 
          (* If we can still build another town, set this as next objective. *)
          (if (get_num_settles color inters Town) < cMAX_TOWNS_PER_PLAYER then
          town_builder structs board color plist else ());
          begin match !goal with
            | ORoad (p1, p2) -> if (can_afford RoadBuy color plist) then
                let () = goal:= NA in 
                Action (BuyBuild (BuildRoad (color, (p1, p2)))) else default_action turn
            | OKnight -> default_action turn
            | OCity loc -> if (can_afford CityBuy color plist) then
                let () = goal:=NA in
                Action (BuyBuild (BuildCity loc)) else default_action turn
            | OTown loc -> if (can_afford TownBuy color plist) then
                let () = goal:=NA in
                Action (BuyBuild (BuildTown loc)) else default_action turn
            | NA -> default_action turn
          end
        
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
