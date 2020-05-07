(** Module for the Uno game AI.  
    The AI plays Uno against the user following the stated game rules.
    It makes random decisions.   *)

(** [player_turn t] is the command for the next move to be made by the 
    player, depending on the current gamestate t *)
val player_turn : Gamestate.t -> Gamestate.gamer -> Command.command 

(** [choose_color t] is the wildcard color by the player, depending on the 
    current gamestate t *)
val choose_color : Gamestate.t -> Command.color