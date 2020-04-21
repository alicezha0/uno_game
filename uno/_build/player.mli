(** Module for the Uno game AI.  
    The AI plays Uno against the user following the stated game rules.
    It makes random decisions.   *)


type card_name = string 

type command

(** [player hand card] is the command for the next move to be made by the 
    player, depending on their current hand [hand], and the last card 
    played [card]. *)
val player_turn : card_name list -> card_name -> command 