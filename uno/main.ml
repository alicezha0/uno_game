open Gamestate
open Command
open Player

let c_empty =
  "\nYour command was empty. If you are having trouble with the game, please \
   refer to the rules with 'Rules' or the commands with 'Commands'. Otherwise, \
   please enter a valid command: \n"

let c_malformed =
  "\nYour command was malformed. If you are having trouble with the game, \
   please refer to the rules with 'Rules' or the commands with 'Commands'. \
   Otherwise, please enter a valid command: \n"

let c_empty_color =
  "\nYour color was empty. If you are having trouble with the game, please \
   refer to the rules with 'Rules' or the commands with 'Commands'. Otherwise, \
   please enter a valid color (Red, Yellow, Green, or Blue): \n"

let c_malformed_color =
  "\nYour color was malformed. If you are having trouble with the game, \
   please refer to the rules with 'Rules' or the commands with 'Commands'. \
   Otherwise, please enter a valid color (Red, Yellow, Green, or Blue): \n"

let end_game =
  "\nSorry to see you go.... Hope you liked the game! \
   Best: Caroline, Nat, Alice :)"

let print_for_user gs =
  (print_endline ("\nThe card on top of the discard pile is: " ^ 
                  Gamestate.last_card_played gs);
   print_endline ("The color you should match is: " ^ 
                  (Gamestate.color_state gs));
   print_endline ("Your current hand has: " ^ 
                  String.concat ", " (Gamestate.hand gs User));
   print_endline ("The other player has " ^ 
                  (string_of_int (Gamestate.hand_size gs Player)) ^ 
                  " card(s).");
   print_endline ("\nIt is your turn to play!");
   print_endline "\nEnter Your Command: \n";)

let print_for_player gs =
  (print_endline ("\nThe AI is playing. Please be patient....."))

let winning gs gamer =
  if Gamestate.win_or_not gs gamer then
    if gamer = User then 
      ((print_endline ("\nYou have won the game! Congratulations! 
      \nThanks for playing!
      \nBest: Alice, Caroline, Nat")); 
       true)
    else ((print_endline ("\nThe AI has won the game. Sucks to be you. 
    \nYou should play again to redeem your honor.\n")); true)
  else false

let uno2_valid_print gamer =
  if gamer = User then "\nYou called uno on the AI. The AI has been forced \
                        to draw 4 cards."
  else "\nThe AI called uno on you. You have been penalized 4 cards."

let uno_valid_print gamer = 
  if gamer = User then "\nYou have called Uno successfully! One card away from \
                        winning!"
  else "\nThe AI has called uno! Looks like you're gonna lose...unless...?"

let print_tally gs =
  let tally_num = string_of_int (Gamestate.current_tally_num gs) in 
  if tally_num = "0" then () else
  if (Gamestate.current_tally_gamer gs) = User 
  then print_endline ("\nThe tally is now " ^ tally_num 
                      ^ " against you. If you don't have a +2 or +4 card, \
                         you should Draw.")
  else
    print_endline ("\nThe tally is now " ^ tally_num ^ " against the AI.")

let turn (gamer:gamer) (gamer_lst: gamer list) (phr:string) : gamer list =
  let once_turn = (List.tl gamer_lst) @ ((List.hd gamer_lst)::[]) in
  if phr = "" || phr = "Wild" then once_turn else
    let index_space = String.index phr ' ' in
    let s_or_r = phr.[index_space+1] in
    if s_or_r = 'R' then
      (List.rev (List.tl gamer_lst)) @ ((List.hd gamer_lst)::[])
    else if s_or_r = 'S' then
      (List.tl once_turn) @ ((List.hd once_turn)::[])
    else
      once_turn

let rec parse_check str = 
  match (parse str) with 
  | command -> command
  | exception Command.Empty -> print_endline c_empty; 
    parse_check (read_line ())
  | exception Command.Malformed -> print_endline c_malformed; 
    parse_check (read_line ())

let rec parse_color str =
  match (Command.parse_color str) with 
  | color -> color
  | exception Command.Empty -> print_endline c_empty_color; 
    parse_color (read_line ())
  | exception Command.Malformed -> print_endline c_malformed_color; 
    parse_color (read_line ())

let pick_color str =
  (print_endline "\nYou played a wild card, which means you get to choose the \
                  \nnext color to be played. Please enter the next color \
                  \n(Red, Yellow, Green, Blue):\n");
  parse_color (read_line ())

let rec recurse_command gs gamer (gamer_lst:Gamestate.gamer list) win =
  let _ = if win then exit 0 else
    if gamer = User then print_for_user gs else print_for_player gs in
  let command_from_gamer = if gamer = Player then Player.player_turn gs
    else (parse_check (read_line ())) in
  match command_from_gamer with 
  | Command.Draw -> 
    let turn_g = turn gamer gamer_lst "" in 
    recurse_command (c_draw gs gamer 1) (List.hd turn_g) turn_g win
  | Command.Play phrase -> c_play gs gamer gamer_lst phrase win
  | Command.Uno phrase -> c_uno gs gamer gamer_lst phrase win
  | Command.Uno2 called_on -> c_uno2 gs gamer called_on gamer_lst win
  | Command.Rules -> 
    print_endline Command.rules; recurse_command gs gamer gamer_lst win
  | Command.Commands -> 
    print_endline Command.commands; recurse_command gs gamer gamer_lst win
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
and c_play gs gamer gamer_lst phr win =
  let pick = 
    if gamer = Player then c_color_ai gs phr else
      (if phr = "Wild" || phr = "Wild +4" then pick_color "" else Any) in
  let play_result = Command.play gs gamer phr pick in 
  match play_result with 
  | Legal gamestate -> print_tally gamestate;
    let turn_g = turn gamer gamer_lst phr in
    recurse_command gamestate (List.hd turn_g) turn_g (winning gamestate gamer)
  | Illegal string -> print_endline string; 
    recurse_command gs gamer gamer_lst win

(** [c_uno gs gamer phr] is mutually recursive with [recurse_command gs gamer]
    and takes care of situation in which uno (defensive) is being played. 
    If the move is Illegal no uno situation, [gamer] is forced to draw 4 cards 
    and then it will be the other gamer's move.*)
and c_uno gs gamer gamer_lst phr win =
  let pick = 
    if gamer = Player then c_color_ai gs phr else
      (if phr = "Wild" || phr = "Wild +4" then pick_color "" else Any) in
  let uno_result = Command.uno gs gamer phr pick in 
  match uno_result with
  | Legal gamestate -> print_tally gamestate; 
    print_endline (uno_valid_print gamer);
    let turn_g = turn gamer gamer_lst phr in
    recurse_command gamestate (List.hd turn_g) turn_g win
  | Illegal string -> 
    if string = "nouno" then 
      (print_endline "\nYou did not call a valid uno. You've been penalized \
                      four cards.";
       let turn_g = turn gamer gamer_lst phr in 
       recurse_command (c_draw gs gamer 4) (List.hd turn_g) turn_g win)
    else (print_endline string; recurse_command gs gamer gamer_lst win)

and c_color_ai gs phr =
  if phr = "Wild" || phr = "Wild +4" then Player.choose_color gs else Any

(** [c_uno2 gs gamer] is mutually recursive with [recurse_command gs gamer]
    and takes care of the situation in which [gamer] calls uno (offensive) on
    the other gamer. If the move is Illegal, then [gamer] is forced to draw 4 
    cards and the turn goes to the other gamer. *)
and c_uno2 gs gamer called_on gamer_lst win =
  let uno2_result = Command.uno2 gs gamer called_on in 
  let turn_g = turn gamer gamer_lst "" in
  match uno2_result with 
  | Legal gamestate -> (print_endline (uno2_valid_print gamer));
    recurse_command gamestate (List.hd turn_g) turn_g win
  | Illegal string -> print_endline string;
    recurse_command (c_draw gs gamer 4) (List.hd turn_g) turn_g win

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline 
    "\nWelcome to Uno! This game was coded by: Caroline Chu, Nishat Peuly, and \
     Alice Zhao.
    \nPlease take some time to review the rules of this game with the command 
    \n'Rules' or learn the commands of this game with the command 'Commands'
    \nHave Fun and Good Luck!\n";
  let begin_uno = Gamestate.from_json 
      (Yojson.Basic.from_file "init_small.json") 7 in 
  recurse_command begin_uno User [User;Player] false

(* Execute the game engine. *)
let () = main ()