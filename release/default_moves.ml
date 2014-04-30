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

(* Applies the default move for req, and returns the new game state
	after doing this. *)
let default_move ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) = 
	match req with
	| InitialRequest -> 
		let structs' = min_valid_init (map, structs, deck, discard, robber) color in
			(None, ((map, structs', deck, discard, robber), plist, turn, (color, req)))
	| DiscardRequest -> 
		let (next', plist') = min_valid_discard_full color plist turn.active in
			(None, ((map, structs, deck, discard, robber), plist', turn, next'))
    | RobberRequest -> 
  		let (robber', plist') = min_valid_robber turn.active structs robber plist Move_Robber in
  			(None, ((map, structs, deck, discard, robber'), plist', turn, (color, req)))
    | TradeRequest -> 
    	(None, ((map, structs, deck, discard, robber), plist, turn, (color, req)))
	| _ -> failwith "unimplemented"