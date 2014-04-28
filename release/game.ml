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

let handle_move ((map, structs, deck, discard, robber), 
    plist, turn, (color, req)) m =
  let board = (map, structs, deck, discard, robber) in 
  match m with
  | InitialMove l -> begin
    match req with
    | InitialRequest ->
      let structs' = (init_request board l color) in 
      (None, ((map, structs', deck, discard, robber), plist, turn, (color, req)))
    | _ -> let structs = min_valid board m color
  in (None, ((map, structs, deck, discard, robber), plist, turn, (color, req)))
  end
  | RobberMove rm -> begin 
    match req with
    | RobberRequest -> do_robber_move rm board plist
    | _ -> failwith "unimplemented"
  end 
  | DiscardMove (dm) -> let plist'= discard_request color plist dm in 
  (None, (board, plist', turn, (color, req)))
  | _ -> failwith "unimplemented"

let presentation s = failwith "Were not too much to pay for birth."
