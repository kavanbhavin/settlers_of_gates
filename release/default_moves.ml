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
open End_turn

(* Applies the default move for req, and returns the new game state
	after doing this. *)
let default_move ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) : (state outcome) * move = 
	match req with
	| InitialRequest -> 
		let (structs', road') = min_valid_init (map, structs, deck, discard, robber) color in
		let next' = update_init_turn (List.length (snd structs')) color 
	in ((None, ((map, structs', deck, discard, robber), plist, (new_turn (next_turn turn.active)), next')),
		InitialMove road')
	| DiscardRequest -> 
		let (next', plist', discarded) = min_valid_discard_full color plist turn.active in
			((None, ((map, structs, deck, discard, robber), plist', turn, next')), 
				DiscardMove discarded)
    | RobberRequest -> 
  	   begin match (min_valid_robber turn.active structs robber plist) with 
		| Some (robber', plist', move') -> ((None, ((map, structs, deck, discard, robber'), plist', turn, (next_turn color, ActionRequest))),
					move')
		| None -> failwith "robber failed on min_valid_robber"
  	   end 
    | TradeRequest -> let turn' = update_turn_after_trade turn in 
    	((None, ((map, structs, deck, discard, robber), plist, turn', (turn.active, ActionRequest))),
    		TradeResponse false)
	| ActionRequest -> 
		match turn.dicerolled with 
		| Some (_) -> 
			begin match end_turn turn plist with 
				| Some (turn', plist') -> 
					((None, ((map, structs, deck, discard, robber), plist', turn', (next_turn color, ActionRequest))),
						Action EndTurn)
				| None -> failwith "this should never happen"
			end 
		| None -> let (next', plist', turn') = (roll_dice plist (map, structs, deck, discard, robber) color turn)
			in ((None, ((map, structs, deck, discard, robber), plist', turn', next')), Action RollDice) 
