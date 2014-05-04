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
  (* A pointer to the resources we need to meet the current objective. *)
  let res_needed = ref (0, 0, 0, 0, 0)

  (* A running list of which resources we own generator tiles for. *)
  let (res_tiles : resource list ref) = ref []

  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = 
    (res_tiles := []);
     goal:=NA;
     res_needed:=(0,0,0,0,0)

  (* Uses weightings from botutil to pick an initial move. *)
  let choose_init_move (inters, roads) board color : line = 
    let weighted_inters = sort_locs inters board !res_tiles in
    let rec pick_move wi : line = 
      match wi with
      | [] -> (0, 0)
      | (_, h, score)::t -> match get_valid_road h (inters, roads) color with
        | None -> pick_move t
        | Some _ ->
          let () = print_endline ("score: "^(string_of_int score)^" at: "^(string_of_int h)) in
          (add_res h res_tiles board); 
            get_init_road (inters, roads) !res_tiles board color h in
    let (p1, p2) = pick_move weighted_inters in
    let () = print_endline ("line: "^(string_of_int p1)^" to "^(string_of_int p2)) in
    (p1, p2)

   (* We have yet to build the maximum number of towns.
    Work towards our next town!. *)
   let town_builder (inters, roads) board color plist : unit = 
    let (_, ((b,w,o,g,l), _), _) = get_player color plist in
    (goal:= town_move (inters, roads) !res_tiles board (b,w,o,g,l) color)

   (* We have yet to build the maximum number of cities.
  Work towards our next city!. *)
  let city_builder (inters, roads) board color plist : unit = 
   (goal:= city_move (inters, roads) !res_tiles board color)

  (* We want to build a card now.
  Work towards our next card!. *)
  let card_builder () : unit = 
   (goal:= OCard)

   (* Default action move: either roll dice or end turn. *)
   let default_action turn = 
    if is_none turn.dicerolled then Action(RollDice) else Action(EndTurn)

  (* Invalid moves are overridden in game *)
  let handle_request ((board, plist, turn, next) : state) : move =
    let (color, request) = next in
    let (map, structs, deck, discard, robber) = board in 
    let (inters, roads) = structs in
    let (hexes, ports) = map in 
    match request with
      | InitialRequest -> InitialMove (choose_init_move structs board color) 
      | RobberRequest -> let (temp, _) = (get_robber_move hexes inters color robber plist !res_needed) in 
          let () = print_endline ("robbing to: "^(string_of_int temp)) in
          RobberMove(get_robber_move hexes inters color robber plist !res_needed)
      | DiscardRequest-> let (b,w,o,g,l) = (get_discard_res (get_res color plist) !res_needed) in
        let () = print_endline ((string_of_int b)^(string_of_int w)^(string_of_int o)^(string_of_int g)^(string_of_int l)) in
        DiscardMove (get_discard_res (get_res color plist) !res_needed)
      | TradeRequest -> TradeResponse(false)
      | ActionRequest -> 
          (* If we can still build another town, set this as next objective. *)
          (if (get_num_settles color inters Town) < cMAX_TOWNS_PER_PLAYER then
          town_builder structs board color plist else 
          (* If not, but we can still build a city, set this as the next objective. *)
          if (get_num_settles color inters City) < cMAX_CITIES_PER_PLAYER then
          city_builder structs board color plist else 
          (* We can't build a city or a town...might as well just make a card! *)
          card_builder ());
          (* Update the resource goal based on the objective we've decided on. *)
          (res_needed:= get_goal_res !goal);

          (* If we cannot afford our goal, see if there's a way to get there. *)
          if not (can_pay_cost !res_needed color plist) then
            match try_for_res hexes structs robber !res_needed color plist !goal with
              | SPlayerTrade (i, c1, c2) -> default_action turn
              | SMaritimeTrade (res1, res2) -> Action (MaritimeTrade (res1, res2))
              | SBuildCard -> Action (BuyBuild BuildCard)
              | SPlayYoP (r1, r2) -> Action (PlayCard (PlayYearOfPlenty (r1, Some r2)))
              | SPlayMonopoly r1 -> Action (PlayCard (PlayMonopoly r1))
              | SPlayKnight mov -> Action (PlayCard (PlayKnight mov))
              | SPlayRoadBuild (p1, p2) -> Action (PlayCard (PlayRoadBuilding ((color, (p1, p2)), None)))
              | NoStrategy -> default_action turn
        else 
          begin match !goal with
            | ORoad (p1, p2) -> if (can_afford RoadBuy color plist) then
                let () = goal:= NA in 
                let () = res_needed:= (0,0,0,0,0) in
                Action (BuyBuild (BuildRoad (color, (p1, p2)))) else default_action turn
            | OCard -> if (can_afford CardBuy color plist) then
                let () = goal:=NA in
                let () = res_needed:=(0,0,0,0,0) in 
                Action (BuyBuild (BuildCard)) else default_action turn
            | OCity loc -> if (can_afford CityBuy color plist) then
                let () = goal:=NA in
                let () = res_needed:= (0,0,0,0,0) in
                Action (BuyBuild (BuildCity loc)) else default_action turn
            | OTown loc -> if (can_afford TownBuy color plist) then
                let () = goal:=NA in
                let () = res_needed:= (0,0,0,0,0) in
                (add_res loc res_tiles board); 
                Action (BuyBuild (BuildTown loc)) else default_action turn
            | NA -> default_action turn
          end
        
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
