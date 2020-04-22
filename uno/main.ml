open Gamestate
open Command
open Player


let turn gamer =
  if gamer = User then Player else User

let rec recurse_command (gs:Gamestate.t) (gamer:Gamestate.gamer) =
  print_endline "Enter Your Command: ";
  match Command.parse (read_line ()) with 
  | Command.Draw -> c_draw gs
  | Command.Play phrase -> c_play gs phrase
  | Command.Uno phrase -> c_uno gs phrase
  | Command.Uno2 -> c_uno2
  | Command.Rules -> c_rules
  | Command.Commands -> c_commands
  | exception Command.Empty ->
    print_endline "Your command was empty."; recurse_command gs gamer
  | exception Command.Malformed -> 
    print_endline "Your command was malformed."; recurse_command gs gamer


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline 
    "\nWelcome to Uno! This game was coded by: Caroline Chu, Nishat Peuly, and Alice Zhao.\n
  Please take some time to review the rules of this game with the command 'Rules' or \n
  learn the commands of this game with the command 'Commands'\n
  Have Fun and Good Luck!\n";
  let begin_uno_game = Gamestate.from_json in 
  recurse_command begin_uno_game User

(* Execute the game engine. *)
let () = main ()