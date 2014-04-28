open Definition
open Constant
open Util
open Print
open Is_valid
open Build
open Robber
open Init_move
open Discard

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
    | RobberRequest -> do_robber_move rm board plist
    | _ -> default_move game
  end 
  | DiscardMove (dm) -> begin
  	match req with
  	| DiscardRequest ->
  		let plist'= discard_request color plist dm in 
  		(None, (board, plist', turn, (color, req)))
  	| _ -> default_move game
  end
  | _ -> failwith "unimplemented"

let presentation s = failwith "Were not too much to pay for birth."
