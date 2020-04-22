open Gamestate
open Command
open Player

let c_empty =
  "Your command was empty. If you are having trouble with the game, please \n
  refer to the rules with 'Rules' or the commands with 'Commands'."

let c_malformed =
  "Your command was malformed. If you are having trouble with the game, \n
  please refer to the rules with 'Rules' or the commands with 'Commands'."

let end_game =
  "Sorry to see you go.... \n
   Hope you liked the game! \n
   Best: Caroline, Nat, Alice :)"

let turn gamer =
  if gamer = User then Player else User

let rec recurse_command (gs:Gamestate.t) (gamer:Gamestate.gamer) =
  print_endline "Enter Your Command: ";
  match Command.parse (read_line ()) with 
  | exception Command.Empty ->
    print_endline c_empty; recurse_command gs gamer
  | exception Command.Malformed -> 
    print_endline c_malformed; recurse_command gs gamer
  | Command.Draw -> recurse_command (c_draw gs gamer 1) (turn gamer)
  | Command.Play phrase -> c_play gs gamer phrase
  | Command.Uno phrase -> c_uno gs gamer phrase
  | Command.Uno2 -> c_uno2 gs gamer
  | Command.Rules -> 
    print_endline Command.rules; recurse_command gs gamer
  | Command.Commands -> 
    print_endline Command.commands; recurse_command gs gamer
  | Command.Quit ->
    print_endline end_game; exit 0

(** [c_draw gs gamer num] is a new Gamestate in which the [gamer] has drawn or
    been forced to draw [num] cards up. *)
and c_draw gs gamer num = 
  let draw_result = Command.draw gs gamer num in 
  match draw_result with
  | Legal gamestate -> gamestate
  | _ -> print_endline "not possible"; gs

(** [c_play gs gamer phr] is mutually recursive with [recurse_command gs gamer]
    and takes care of situation in which a card [phr] is being played by
    [gamer]. If the move is Illegal, the error statement is printed and the
    [gamer] has another chance to play.*)
and c_play gs gamer phr =
  let play_result = Command.play gs gamer phr in 
  match play_result with 
  | Legal gamestate -> recurse_command gamestate (turn gamer)
  | Illegal string -> print_endline string; recurse_command gs gamer

(** [c_uno gs gamer phr] is mutually recursive with [recurse_command gs gamer]
    and takes care of situation in which uno (defensive) is being played. 
    If the move is Illegal no uno situation, [gamer] is forced to draw 4 cards 
    and then it will be the other gamer's move.*)
and c_uno gs gamer phr =
  let uno_result = Command.uno gs gamer phr in 
  match uno_result with
  | Legal gamestate -> recurse_command gamestate (turn gamer)
  | Illegal string -> 
    if string = "nouno" then 
      (print_endline "You did not call a valid uno. You've been penalized four
      cards.";
       recurse_command (c_draw gs gamer 4) (turn gamer))
    else print_endline string; recurse_command gs gamer

(** [c_uno2 gs gamer] is mutually recursive with [recurse_command gs gamer]
    and takes care of the situation in which [gamer] calls uno (offensive) on
    the other gamer. If the move is Illegal, then [gamer] is forced to draw 4 
    cards and the turn goes to the other gamer. *)
and c_uno2 gs gamer =
  let uno2_result = Command.uno2 gs gamer (turn gamer) in 
  match uno2_result with 
  | Legal gamestate -> recurse_command gamestate (turn gamer)
  | Illegal string -> print_endline string;
    recurse_command (c_draw gs gamer 4) (turn gamer)

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