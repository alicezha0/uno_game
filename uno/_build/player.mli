(** Module for the Uno game AI.  
    The AI plays Uno against the user following the stated game rules.
    It makes random decisions.   *)


type card_name = string 

type command

type t

(** [player_turn t] is the command for the next move to be made by the 
    player, depending on the current gamestate t *)
val player_turn : t -> command 