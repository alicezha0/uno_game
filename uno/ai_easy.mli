(** Module for the Uno game AI - easy.  
    The AI plays Uno against the user following the stated game rules.
    It makes decisions guided by rules stated in player3.ml.   *)

(** [player_turn t gamer] is the command for the next move to be made by the 
    player, depending on the current gamestate t *)
val player_turn : Gamestate.t -> Gamestate.gamer -> Command.command 

(** [choose_color t] is the wildcard color by the player, depending on the 
    current gamestate t *)
val choose_color : Gamestate.t -> Command.color