open Gamestate
open Command
open Player

let c_empty =
  "\nYour command was empty. If you are having trouble with the game, please 
  \nrefer to the rules with 'Rules' or the commands with 'Commands'. Otherwise,
  \nplease enter a valid command: \n"

let c_malformed =
  "\nYour command was malformed. If you are having trouble with the game, 
  \nplease refer to the rules with 'Rules' or the commands with 'Commands'. 
  \nOtherwise, please enter a valid command: \n"

let end_game =
  "\nSorry to see you go.... 
  \nHope you liked the game! 
  \nBest: Caroline, Nat, Alice :)"

let turn gamer =
  if gamer = User then Player else User

let print_for_user gs =
  (print_endline ("\nThe last card played was: " ^ Gamestate.last_card_played gs);
   print_endline ("Your current hand has: " ^ 
                  (String.concat " " (Gamestate.hand gs User)));
   print_endline ("The other player has " ^ 
                  (string_of_int (Gamestate.hand_size gs Player)) ^ 
                  " number of card(s).");
   print_endline ("\nIt is your turn to play!");
   print_endline "\nEnter Your Command: \n";)

let print_for_player gs =
  (print_endline ("\nThe AI is playing. Please be patient....."))

let winning gs gamer =
  if Gamestate.win_or_not gs gamer then
    if gamer = User then 
      ((print_endline ("\nYou have won the game! Congratulations! Thanks for playing. Bye-bye.\n")); 
       true)
    else ((print_endline ("\nThe AI has won the game. You suck. Bye.\n")); true)
  else false

let rec parse_check str = 
  match (parse str) with 
  | command -> command
  | exception Command.Empty -> print_endline c_empty; 
    parse_check (read_line ())
  | exception Command.Malformed -> print_endline c_malformed; 
    parse_check (read_line ())

let uno2_valid_print gamer =
  if gamer = User then "The AI called uno on you. You have been penalized 4 cards. 
\n Get wrecked."
  else "You called uno on the AI. The AI has been forced to draw 4 cards. litty"

let uno_valid_print gamer = 
  if gamer = User then "You have called Uno successfully!"
  else "The AI has called uno! Are you just gonna lose?"

let rec recurse_command (gs:Gamestate.t) (gamer:Gamestate.gamer) (win:bool) =
  let _ = if win then exit 0 else
    if gamer = User then print_for_user gs else print_for_player gs in
  let command_from_gamer = if gamer = Player then Player.player_turn gs
    else (parse_check (read_line ())) in
  match command_from_gamer with 
  | Command.Draw -> recurse_command (c_draw gs gamer 1) (turn gamer) win
  | Command.Play phrase -> c_play gs gamer phrase win
  | Command.Uno phrase -> c_uno gs gamer phrase win
  | Command.Uno2 -> c_uno2 gs gamer win
  | Command.Rules -> 
    print_endline Command.rules; recurse_command gs gamer win
  | Command.Commands -> 
    print_endline Command.commands; recurse_command gs gamer win
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
and c_play gs gamer phr win =
  let play_result = Command.play gs gamer phr in 
  match play_result with 
  | Legal gamestate -> recurse_command gamestate (turn gamer) (winning gamestate gamer)
  | Illegal string -> print_endline string; recurse_command gs gamer win

(** [c_uno gs gamer phr] is mutually recursive with [recurse_command gs gamer]
    and takes care of situation in which uno (defensive) is being played. 
    If the move is Illegal no uno situation, [gamer] is forced to draw 4 cards 
    and then it will be the other gamer's move.*)
and c_uno gs gamer phr win =
  let uno_result = Command.uno gs gamer phr in 
  match uno_result with
  | Legal gamestate -> print_endline (uno_valid_print gamer) ;
    recurse_command gamestate (turn gamer) win
  | Illegal string -> 
    if string = "nouno" then 
      (print_endline "You did not call a valid uno. You've been penalized four cards.";
       recurse_command (c_draw gs gamer 4) (turn gamer)) win
    else print_endline string; recurse_command gs gamer win

(** [c_uno2 gs gamer] is mutually recursive with [recurse_command gs gamer]
    and takes care of the situation in which [gamer] calls uno (offensive) on
    the other gamer. If the move is Illegal, then [gamer] is forced to draw 4 
    cards and the turn goes to the other gamer. *)
and c_uno2 gs gamer win =
  let uno2_result = Command.uno2 gs gamer (turn gamer) in 
  match uno2_result with 
  | Legal gamestate -> (print_endline (uno2_valid_print gamer)); 
    recurse_command gamestate (turn gamer) win
  | Illegal string -> print_endline string;
    recurse_command (c_draw gs gamer 4) (turn gamer) win

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline 
    "\nWelcome to Uno! This game was coded by: Caroline Chu, Nishat Peuly, and Alice Zhao.
    \nPlease take some time to review the rules of this game with the command 
    \n'Rules' or learn the commands of this game with the command 'Commands'
    \nHave Fun and Good Luck!\n";
  let begin_uno = Gamestate.from_json (Yojson.Basic.from_file "init.json") 7 in 
  recurse_command begin_uno User false

(* Execute the game engine. *)
let () = main ()