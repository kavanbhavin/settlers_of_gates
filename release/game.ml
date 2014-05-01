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

let handle_move ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) m =
  let board = (map, structs, deck, discard, robber) in 
  let game = (board, plist, turn, (color, req)) in
  match m with
  | InitialMove l -> begin
    match req with
    | InitialRequest ->
      let (intersections', roads') = (init_request board l color) in 
      let (color', req') = update_init_turn (List.length roads') color 
  in (None, ((map, (intersections', roads'), deck, discard, robber), plist, (new_turn color'), (color', req')))
    | _ -> default_move game
  end
  | RobberMove rm -> begin 
    match req with
    | RobberRequest -> 
    	begin match do_robber_move turn.active rm structs robber plist with 
          | Some (robber', plist') -> (None, ((map, structs, deck, discard, robber'), plist', turn, (color, ActionRequest)))
          | None -> 
          begin match min_valid_robber turn.active structs robber plist with 
                      | Some (robber', plist') -> (None, ((map, structs, deck, discard, robber'), plist', turn, (color, ActionRequest)))
                      | None -> failwith "min_valid_robber failed to provied a valid robber move"
          end 
        end 
    | _ -> default_move game
  end 
  | DiscardMove (dm) -> begin
  	match req with
  	| DiscardRequest ->
  		let (next', plist')= discard_request color plist dm turn.active in 
  		(None, (board, plist', turn, (next')))
  	| _ -> default_move game
  end
    | TradeResponse res -> begin
    	match req with
      (* fix color changing*)
    	| TradeRequest ->
	    	let (origin, trade) = get_trade_info turn in
	    	let plist' = handle_trade res origin trade plist in
	    	let turn' = update_turn_after_trade turn in
	    	(None, (board, plist',turn',(origin, ActionRequest)))
	    | _ -> default_move game
    end
    | Action act -> begin
    	match req with
    	| ActionRequest -> begin
    		match act with
        | RollDice -> let (next', plist', turn') = roll_dice plist board color turn
      		in (None, (board, plist', turn', next'))
		    | DomesticTrade tr ->
		    	if not (valid_trade tr color plist) || turn.tradesmade >= cNUM_TRADES_PER_TURN 
		    		then default_move game else
		    	let (turn', next') = update_turn_before_trade turn (color,req) tr color plist in
		    	(None, (board, plist, turn', next'))
		    | BuyBuild build -> 
		    	begin match (doBuild game build) with 
              | Some (x) -> x
              | None -> default_move game 
          end 
        | PlayCard card ->
          begin match doPlay game card with 
              | Some x -> x
              | None -> default_move game 
          end
        | EndTurn ->
          begin match end_turn turn plist with 
              | Some (turn', plist') -> 
              (None, (board, plist', turn', (turn'.active , ActionRequest)))
              | None -> default_move game 
          end 
    		| _ -> failwith "unimplemented"
    		end
    	| _ -> default_move game
    	end

let presentation s = failwith "Were not too much to pay for birth."
