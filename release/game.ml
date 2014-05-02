open Definition
open Constant
open Util
open Print
open Is_valid
open Build
open Robber
open Init_move
open Discard
open Trade
open Roll_dice
open Play_card
open Default_moves
open End_turn

type game = state

let state_of_game g = g
let game_of_state s = s


let init_game () = game_of_state (gen_initial_state())

let handle_move' ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) m : (game outcome) * move =
  let board = (map, structs, deck, discard, robber) in 
  let game = (board, plist, turn, (color, req)) in
  match m with
  | InitialMove l -> begin
    match req with
    | InitialRequest ->
      let ((intersections', roads'), road') = (init_request board l color) in 
      let (color', req') = update_init_turn (List.length roads') color 
  		in ((None, ((map, (intersections', roads'), deck, discard, robber), plist, (new_turn color'), (color', req'))),
  			(InitialMove road'))
    | _ -> default_move game
  end
  | RobberMove rm -> begin 
    match req with
    | RobberRequest -> 
    	begin match do_robber_move turn.active rm structs robber plist with 
          | Some (robber', plist', move') -> ((None, ((map, structs, deck, discard, robber'), plist', turn, (color, ActionRequest))),
          	move')
          | None -> 
          begin match min_valid_robber turn.active structs robber plist with 
              | Some (robber', plist', move') -> ((None, ((map, structs, deck, discard, robber'), plist', turn, (color, ActionRequest))),
              		move')
              | None -> failwith "min_valid_robber failed to provied a valid robber move"
          end 
        end 
    | _ -> default_move game
  end 
  | DiscardMove (dm) -> begin
  	match req with
  	| DiscardRequest ->
  		let (next', plist', cost_found)= discard_request color plist dm turn.active in 
  		((None, (board, plist', turn, (next'))), DiscardMove cost_found)
  	| _ -> default_move game
  end
    | TradeResponse res -> begin
    	match req with
    	| TradeRequest ->
	    	let (origin, trade) = get_trade_info turn in
	    	let plist' = handle_trade res origin trade plist in
	    	let turn' = update_turn_after_trade turn in
	    	((None, (board, plist',turn',(origin, ActionRequest))), m)
	    | _ -> default_move game
    end
    | Action act -> begin
    	match req with
    	| ActionRequest -> begin
    		match act with
        | RollDice -> let (next', plist', turn') = roll_dice plist board color turn
      		in ((None, (board, plist', turn', next')), Action RollDice)
	    | DomesticTrade tr ->
	    	if not (valid_trade tr color plist) || turn.tradesmade >= cNUM_TRADES_PER_TURN 
	    		then default_move game else
	    	let (turn', next') = update_turn_before_trade turn (color,req) tr color plist in
	    		((None, (board, plist, turn', next')), m)
	    | BuyBuild build -> 
	    	begin match (doBuild game build) with 
          	| Some (x) -> (x, m)
          	| None -> default_move game 
      end 
        | PlayCard card ->
          begin match doPlay game card with 
              | Some x -> (x, m)
              | None -> default_move game 
          end
        | EndTurn ->
          begin match end_turn turn plist with 
              | Some (turn', plist') -> 
              		((None, (board, plist', turn', (turn'.active , ActionRequest))), m)
              | None -> default_move game 
          end 
    	| MaritimeTrade mt ->
          begin match make_sea_trade map structs color mt plist with
           	  | Some plist' ->
           	  	((None, (board, plist', turn, (color, req))), m)
           	  | None -> default_move game
    	  end end
    	| _ -> default_move game
    	end

(* Handles move, then prints new screen, then returns it. *)
let handle_move ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) m = 
let temp = handle_move' ((map, structs, deck, discard, robber), 
	plist, turn, (color, req)) m in
let ((winner, state) , m) = temp in 
match winner with
	| Some _ -> (winner, state)
	| None ->
		(print_update color m state); (winner, state)

let presentation (board, plist, turn, (color, req)) = 
  let plist' = List.map (fun (color', (inventory, cards), trophies) -> 
    let new_cards = 
        if color' = color 
        then cards
        else hide cards
in (color', (inventory, new_cards), trophies) ) plist 
in (board, plist', turn, (color, req)) 







