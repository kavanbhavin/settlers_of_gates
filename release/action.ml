open Util
open Definition 
open Constant
open Own_util
let doAction action = 
	match action with 
 			| RollDice -> let x = random_roll () in  
 			if x = cROBBER_ROLL
 			then failwith "do robber shit"
 			else hex_of_roll x  
            | MaritimeTrade (maritimetrade) -> failwith "maritimetrade"
            | DomesticTrade (trade) -> failwith "trade"
            | BuyBuild (build) -> failwith "BuyBuild"
            | PlayCard (playcard) -> failwith "playcard"
            | EndTurn -> failwith "EndTurn"
