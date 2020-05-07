open Gamestate
open Command
open Player
open Player2 (*rename*)
open Player3 (*rename*)

type ai_diff = Easy | Medium | Hard

type record = {gamer : Gamestate.gamer; diff : ai_diff}

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

let print_for_user gs gamer =
  (print_endline ("\nThe card on top of the discard pile is: " ^ 
                  Gamestate.last_card_played gs);
   print_endline ("The color you should match is: " ^ 
                  (Gamestate.color_state gs));
   print_endline ("Your current hand has: " ^ 
                  String.concat ", " (Gamestate.hand gs User));
   let player_name = 
     if gamer = Player1 then "Spongebob Squarepants"
     else if gamer = Player2 then "Patrick Star"
     else "Mr. Krabs" in 
   print_endline (player_name ^ " has " ^ 
                  (string_of_int (Gamestate.hand_size gs gamer)) ^ 
                  " card(s).");
   print_endline ("\nIt is your turn to play!");
   print_endline "\nEnter Your Command: \n";)

let print_for_player gs gamer =
  let g = if gamer = Player1 then "Spongebob Squarepants" 
    else if gamer = Player2 then "Patrick Star"
    else "Mr. Krabs" in
  (print_endline ("\n" ^ g ^ " is playing. Please be patient....."))

let winning gs gamer =
  if Gamestate.win_or_not gs gamer then
    if gamer = User then 
      ((print_endline ("\nYou have won the game! Congratulations! 
      \nThanks for playing!
      \nBest: Alice, Caroline, Nat")); 
       true)
    else if gamer = Player1 then
      ((print_endline ("\nSpongebob Squarepants has won the game. Sucks to be you. 
      \nYou should play again to redeem your honor.\n")); true)
    else if gamer = Player2 then 
      ((print_endline ("\nPatrick Star has won the game. Sucks to be you. 
    \nYou should play again to redeem your honor.\n")); true)
    else
      ((print_endline ("\nMr. Krabs has won the game. Sucks to be you. 
    \nYou should play again to redeem your honor.\n")); true)
  else false

let uno2_valid_print gamer call_on_gamer =
  let uno_on_you = " called uno on you. You have been penalized 4 cards." in
  if gamer = User then 
    let uno_initial_str = "\nYou called uno on " in 
    let uno_next_str = ". He has been forced to draw 4 cards." in 
    if call_on_gamer = Player1 then
      uno_initial_str ^ "Spongebob Squarepants" ^ uno_next_str
    else if call_on_gamer = Player2 then 
      uno_initial_str ^ "Patrick Star" ^ uno_next_str
    else
      uno_initial_str ^ "Mr. Krabs" ^ uno_next_str
  else if gamer = Player1 then
    "\nSpongebob Squarepants" ^ uno_on_you 
  else if gamer = Player2 then 
    "\nPatrick Star" ^ uno_on_you 
  else 
    "\nMr. Krabs" ^ uno_on_you

let uno_valid_print gamer = 
  let ai_str = " has called uno! Looks like you're gonna lose...unless...?" in
  if gamer = User then "\nYou have called Uno successfully! One card away from \
                        winning!"
  else if gamer = Player1 then
    "Spongebob Squarepants" ^ ai_str
  else if gamer = Player2 then 
    "Patrick Star" ^ ai_str 
  else
    "Mr. Krabs" ^ ai_str

let print_tally gs gamer =
  let tally_num = string_of_int (Gamestate.current_tally_num gs) in 
  if tally_num = "0" then () else
    let curr_tally_g = Gamestate.current_tally_gamer gs in
    if curr_tally_g = User 
    then print_endline ("\nThe tally is now " ^ tally_num 
                        ^ " against you. If you don't have a +2 or +4 card, \
                           you should Draw.")
    else if curr_tally_g = Player1 then
      print_endline ("\nThe tally is now " ^ tally_num ^ " against Spongebob.")
    else if curr_tally_g = Player2 then 
      print_endline ("\nThe tally is now " ^ tally_num ^ " against Patrick.")
    else
      print_endline ("\nThe tally is now " ^ tally_num ^ " against Krabs.")

let turn (gamer:gamer) (gamer_lst: record list) (phr:string) : record list =
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
  match (Command.parse str) with 
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

let player_plays (gs:Gamestate.t) (gamer_lst:record list) : Command.command = 
  let difficulty = (List.hd gamer_lst).diff in 
  if difficulty = Easy then Player3.player_turn gs (* rename *)
  else if difficulty = Medium then Player.player_turn gs (* rename *)
  else Player2.player_turn gs (*rename*)

let rec recurse_command gs gamer (gamer_lst:record list) win =
  let _ = 
    if win then exit 0 
    else
    if gamer = User then print_for_user gs gamer
    else print_for_player gs gamer in
  let command_from_gamer = 
    if gamer <> User then player_plays gs gamer_lst
    else (parse_check (read_line ())) in
  match command_from_gamer with 
  | Command.Draw -> 
    let turn_g = turn gamer gamer_lst "" in 
    recurse_command (c_draw gs gamer 1) (List.hd turn_g).gamer turn_g win
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
    if gamer <> User then c_color_ai gs gamer_lst phr else
      (if phr = "Wild" || phr = "Wild +4" then pick_color "" else Any) in
  let next_gamer = (List.hd (List.tl gamer_lst)).gamer in
  let play_result = Command.play gs gamer next_gamer phr pick in 
  match play_result with 
  | Legal gamestate -> print_tally gamestate gamer;
    let turn_g = turn gamer gamer_lst phr in
    recurse_command gamestate (List.hd turn_g).gamer 
      turn_g (winning gamestate gamer)
  | Illegal string -> print_endline string; 
    recurse_command gs gamer gamer_lst win

(** [c_uno gs gamer phr] is mutually recursive with [recurse_command gs gamer]
    and takes care of situation in which uno (defensive) is being played. 
    If the move is Illegal no uno situation, [gamer] is forced to draw 4 cards 
    and then it will be the other gamer's move.*)
and c_uno gs gamer gamer_lst phr win =
  let pick = 
    if gamer <> User then c_color_ai gs gamer_lst phr else
      (if phr = "Wild" || phr = "Wild +4" then pick_color "" else Any) in
  let next_gamer = (List.hd (List.tl gamer_lst)).gamer in
  let uno_result = Command.uno gs gamer next_gamer phr pick in 
  match uno_result with
  | Legal gamestate -> print_tally gamestate gamer; 
    print_endline (uno_valid_print gamer);
    let turn_g = turn gamer gamer_lst phr in
    recurse_command gamestate (List.hd turn_g).gamer turn_g win
  | Illegal string -> 
    if string = "nouno" then 
      (print_endline "\nYou did not call a valid uno. You've been penalized \
                      four cards.";
       let turn_g = turn gamer gamer_lst phr in 
       recurse_command (c_draw gs gamer 4) (List.hd turn_g).gamer turn_g win)
    else (print_endline string; recurse_command gs gamer gamer_lst win)

and c_color_ai gs gamer_lst phr =
  if phr = "Wild" || phr = "Wild +4" then 
    let curr_player_diff = (List.hd gamer_lst).diff in 
    match curr_player_diff with 
    | Easy -> Player3.choose_color gs (* rename *)
    | Medium -> Player.choose_color gs (* rename *)
    | Hard -> Player2.choose_color gs (* rename *)
  else Any

(** [c_uno2 gs gamer] is mutually recursive with [recurse_command gs gamer]
    and takes care of the situation in which [gamer] calls uno (offensive) on
    the other gamer. If the move is Illegal, then [gamer] is forced to draw 4 
    cards and the turn goes to the other gamer. *)
and c_uno2 gs gamer called_on gamer_lst win =
  let uno2_result = Command.uno2 gs gamer called_on in 
  let turn_g = turn gamer gamer_lst "" in
  match uno2_result with 
  | Legal gamestate -> (print_endline (uno2_valid_print gamer called_on));
    recurse_command gamestate (List.hd turn_g).gamer turn_g win
  | Illegal string -> print_endline string;
    recurse_command (c_draw gs gamer 4) (List.hd turn_g).gamer turn_g win

(** 
   Requires: int_lst is a list of integers which are either 1, 2 or 3. No other 
   integer values should be in this list. 
   Returns: a list of players. 
*)
let rec ass_play_to_ai int_lst play_lst =
  match int_lst with 
  | [] -> play_lst 
  | h::t -> 
    if h = 1 then (ass_play_to_ai t (Easy::play_lst))
    else if h = 2 then (ass_play_to_ai t (Medium::play_lst))
    else ass_play_to_ai t (Hard::play_lst)

let rec check_malformed_init (str_lst: string list) = 
  match str_lst with
  | [] -> let len = List.length str_lst in 
    if (len < 1 || len > 3)
    then false 
    else true 
  | h::t -> if (h = "1" || h = "2" || h = "3") then check_malformed_init t 
    else true 

let rec prompt_init_players () =
  print_endline ("\nFirst, before we begin, choose the number of AIs (up to 3) \
                  you would like to play against and the number that \
                  corresponds to its difficulty level (1 = easy, 2 = medium \
                  3 = hard). For example, if you want to play against 2 AIs \
                  of hard difficulty, input '3 3' or if you want to play \
                  against 1 AI of medium difficulty, input '2'. Please enter \
                  your choice: \n");
  let diff = read_line() in 
  let no_empty_spaces = String.trim diff in 
  if no_empty_spaces = "" 
  then prompt_init_players () 
  else let str_lst = String.split_on_char ' ' no_empty_spaces in 
    if (check_malformed_init str_lst) 
    then let int_lst = List.map (int_of_string) str_lst in 
      ass_play_to_ai int_lst []
    else prompt_init_players ()

let rec init_gamer_lst pl_lst = 
  let len = List.length pl_lst in 
  let user_rec = {gamer = User; diff = Easy} in 
  let fir_player_rec = {gamer = Player1; diff = List.hd (pl_lst)} in
  if len = 1 then user_rec::fir_player_rec::[]
  else if len = 2 then 
    let sec_player_rec = {gamer = Player2; diff = List.hd (List.tl pl_lst)} in 
    user_rec::fir_player_rec::sec_player_rec::[]
  else
    let sec_player_rec = {gamer = Player2; diff = List.hd (List.tl pl_lst)} in 
    let thir_player_rec = {gamer = Player3; diff = List.hd (List.rev pl_lst)} in 
    user_rec::fir_player_rec::sec_player_rec::thir_player_rec::[]

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline 
    "\nWelcome to Uno! This game was coded by: Caroline Chu, Nishat Peuly, and \
     Alice Zhao. Please take some time to review the rules of this game with \
     the command 'Rules' or learn the commands of this game with the command \
     'Commands' Have Fun and Good Luck!\n";
  let init_players = prompt_init_players () in
  let num_g = List.length init_players in 
  let begin_uno = Gamestate.from_json 
      (Yojson.Basic.from_file "init_small.json") 7 num_g in 
  recurse_command begin_uno User (init_gamer_lst init_players) false

(* Execute the game engine. *)
let () = main ()