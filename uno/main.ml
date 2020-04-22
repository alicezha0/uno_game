open Gamestate
open Command
open Player

let c_empty =
  "Your command was empty. If you are having trouble with the game, please refer to \n
the rules with 'Rules' or the commands with 'Commands'."

let c_malformed =
  "Your command was malformed. If you are having trouble with the game, please refer to \n
the rules with 'Rules' or the commands with 'Commands'."

let turn gamer =
  if gamer = User then Player else User

let rec recurse_command (gs:Gamestate.t) (gamer:Gamestate.gamer) =
  print_endline "Enter Your Command: ";
  match Command.parse (read_line ()) with 
  | exception Command.Empty ->
    print_endline c_empty; recurse_command gs gamer
  | exception Command.Malformed -> 
    print_endline c_malformed; recurse_command gs gamer
  | Command.Draw -> c_draw gs
  | Command.Play phrase -> c_play gs phrase
  | Command.Uno phrase -> c_uno gs phrase
  | Command.Uno2 -> c_uno2
  | Command.Rules -> 
    print_endline Command.rules; recurse_command gs gamer
  | Command.Commands -> 
    print_endline Command.commands; recurse_command gs gamer
  | Command.Quit ->
    print_endline "ok bye."; exit 0


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline 
    "\nWelcome to Uno! This game was coded by: \n
    Caroline Chu \n
    Nishat Peuly \n
    Alice Zhao.\n
  Please take some time to review the rules of this game with the command \n
  'Rules' or learn the commands of this game with the command 'Commands'\n
  Have Fun and Good Luck!\n";
  let begin_uno = Gamestate.from_json (Yojson.Basic.from_file "init.json") in 
  recurse_command begin_uno User

(* Execute the game engine. *)
let () = main ()