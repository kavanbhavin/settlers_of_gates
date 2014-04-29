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

type game = state

let state_of_game g = g
let game_of_state s = s


let init_game () = game_of_state (gen_initial_state())

(* Applies the default move for req, and returns the new game state
	after doing this. *)
let default_move ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) = 
	match req with
	| InitialRequest -> 
		let structs' = min_valid_init (map, structs, deck, discard, robber) color in
			(None, ((map, structs', deck, discard, robber), plist, turn, (color, req)))
	| DiscardRequest -> 
		let plist' = min_valid_discard_full color plist in
			(None, ((map, structs, deck, discard, robber), plist', turn, (color, req)))
    | RobberRequest -> 
  		let (robber', plist') = min_valid_robber turn (map, structs, deck, discard, robber) plist in
  			(None, ((map, structs, deck, discard, robber'), plist', turn, (color, req)))
    | TradeRequest -> 
    	(None, ((map, structs, deck, discard, robber), plist, turn, (color, req)))
	| _ -> failwith "unimplemented"

let handle_move ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) m =
  let board = (map, structs, deck, discard, robber) in 
  let game = (board, plist, turn, (color, req)) in
  match m with
  | InitialMove l -> begin
    match req with
    | InitialRequest ->
      let structs' = (init_request board l color) in 
      (None, ((map, structs', deck, discard, robber), plist, turn, (color, req)))
    | _ -> default_move game
  end
  | RobberMove rm -> begin 
    match req with
    | RobberRequest -> 
    	let (robber', plist') = do_robber_move turn.active rm board plist in
  		(None, ((map, structs, deck, discard, robber'), plist', turn, (color, req)))
    | _ -> default_move game
  end 
  | DiscardMove (dm) -> begin
  	match req with
  	| DiscardRequest ->
  		let plist'= discard_request color plist dm in 
  		(None, (board, plist', turn, (color, req)))
  	| _ -> default_move game
  end
    | TradeResponse res -> begin
    	match req with
    	| TradeRequest ->
	    	let (origin, trade) = get_trade_info turn in
	    	let plist' = handle_trade res origin trade plist in
	    	let turn' = update_turn_after_trade turn in
	    	(None, (board, plist',turn',(color,req)))
	    | _ -> default_move game
    end
    | Action act -> begin
    	match req with
    	| ActionRequest -> begin
    		match act with
    		| DomesticTrade tr ->
    			if not (valid_trade tr color plist) || turn.tradesmade >= cNUM_TRADES_PER_TURN 
    				then default_move game else
    			let (turn', next') = update_turn_before_trade turn (color,req) tr color plist in
    			(None, (board, plist, turn', next'))
    		| BuyBuild build -> 
    			begin match build with
    				| BuildRoad (col, line) -> 
    					if (can_build_road structs line color plist) then
    					let (structs', plist') = build_road_move structs line color plist in
    						(None, ((map, structs', deck, discard, robber), plist', turn, (color, req)))
    					else default_move game
    				| _ -> failwith "not implemented"
    			end
    		| _ -> failwith "unimplemented"
    		end
    	| _ -> default_move game
    	end

let presentation s = failwith "Were not too much to pay for birth."
