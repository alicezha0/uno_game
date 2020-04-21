open Gamestate 

type card_name = string 

type command

(** [player hand card] is the command for the next move to be made by the 
    player, depending on their current hand [hand], and the last card 
    played [card]. *)
let player_turn card_list card_name = 
  failwith "Unimplemented"