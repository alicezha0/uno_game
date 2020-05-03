(** Module for the Uno game AI - hard.  
    The AI plays Uno against the user following the stated game rules.
    It makes decisions guided by rules stated in player2.ml.   *)

(** [player_turn t] is the command for the next move to be made by the 
    player, depending on the current gamestate t *)
val player_turn : Gamestate.t -> Command.command 