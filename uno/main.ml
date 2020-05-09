open Gamestate
open Command
open Ai_med
open Ai_hard 
open Ai_easy 

(** The type [ai_diff] represents the level of difficulties the user has chosen
    for the AI.*)
type ai_diff = Easy | Medium | Hard

(** The type [record] represents a record list of the gamers and their 
    corresponding difficulties. *)
type record = {gamer : Gamestate.gamer; diff : ai_diff}

let player_name gamer =
  if gamer = Player1 then "Foster"
  else if gamer = Player2 then "Gries"
  else "White"

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
   Best: Caroline, Nat, Alice :)\n"

let rec print_hand_players gs (gamer_lst:record list) acc =
  match gamer_lst with 
  | [] -> acc
  | h::t -> 
    let hand_size = string_of_int (Gamestate.hand_size gs h.gamer) in 
    if h.gamer = User 
    then print_hand_players gs t 
        (acc ^ "\nYou have " ^ hand_size ^ " card(s). ")
    else print_hand_players gs t 
        (acc ^ "\n" ^ player_name h.gamer ^ " has " ^ hand_size ^ " card(s). ")

let print_for_user gs gamer gamer_lst =
  (print_endline ("\nThe card on top of the discard pile is: " ^ 
                  Gamestate.last_card_played gs);
   print_endline ("The color you should match is: " ^ 
                  (Gamestate.color_state gs));
   print_endline ("Your current hand has: " ^ 
                  String.concat ", " (Gamestate.hand gs User));
   let hand_to_str = print_hand_players gs gamer_lst "" in 
   print_endline (hand_to_str);
   print_endline ("\nIt is your turn to play!");
   print_endline "\nEnter Your Command: \n";)

let print_for_player gamer =
  (print_endline ("\n" ^ player_name gamer ^ 
                  " is playing. Please be patient....."))

let winning gs gamer =
  let sec_str = " has won the game. \nYou should play again to redeem \
                 your honor.\n" in 
  if Gamestate.win_or_not gs gamer then
    if gamer = User then 
      ((print_endline ("\nYou have won the game! Congratulations and thanks \
                        for playing! \nBest: Alice, Caroline, Nat")); true)
    else ((print_endline (player_name gamer ^ sec_str)); true)
  else false

let uno2_valid_print gamer call_on_gamer =
  let uno_on_you = " called uno on you. You have been penalized 4 cards." in
  if gamer = User then 
    let uno_initial_str = "\nYou called uno on " in 
    let uno_next_str = ". He has been forced to draw 4 cards." in 
    uno_initial_str ^ player_name call_on_gamer ^ uno_next_str
  else player_name gamer ^ uno_on_you

let uno_valid_print gamer = 
  let ai_str = " has called uno! Looks like you're gonna lose...unless...?" in
  if gamer = User then "\nYou have called Uno successfully! One card away from \
                        winning!"
  else player_name gamer ^ ai_str

let print_tally gs gamer =
  let tally_num = string_of_int (Gamestate.current_tally_num gs) in 
  if tally_num = "0" then () else
    let curr_tally_g = Gamestate.current_tally_gamer gs in
    if curr_tally_g = User 
    then print_endline ("\nThe tally is now " ^ tally_num 
                        ^ " against you. If you don't have a +2 or +4 card, \
                           you should Draw.")
    else print_endline ("\nThe tally is now " ^ tally_num 
                        ^ " against " ^ player_name gamer ^ ".")

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
  let the_gamer = (List.hd gamer_lst).gamer in
  if difficulty = Easy then Ai_easy.player_turn gs the_gamer
  else if difficulty = Medium then Ai_med.player_turn gs the_gamer
  else Ai_hard.player_turn gs the_gamer

let player_drew gamer =
  if gamer = User then () else
    print_endline (player_name gamer ^ " drew card(s).")

let rec recurse_command gs gamer (gamer_lst:record list) win =
  let _ = if win then exit 0 else
    if gamer = User then print_for_user gs gamer gamer_lst
    else print_for_player gamer in
  let command_from_gamer = 
    if gamer <> User then player_plays gs gamer_lst
    else (parse_check (read_line ())) in
  match command_from_gamer with 
  | Command.Draw -> let turn_g = turn gamer gamer_lst "" in 
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
  player_drew gamer;
  let draw_result = Command.draw gs gamer num in 
  match draw_result with
  | Legal gamestate -> gamestate
  | _ -> print_endline "not possible"; gs






(** [c_play gs gamer gamer_lst phr win] takes care of situation in which a 
    card [phr] is being played by [gamer]. If the move is Illegal, the error 
    statement is printed and the [gamer] has another chance to play. Otherwise, 
    [recurse_command] is called. *)
and c_play gs gamer gamer_lst phr win =
  let pick = 
    if gamer <> User then c_color_ai gs gamer_lst phr else
      (if phr = "Wild" || phr = "Wild +4" then pick_color "" else Any) in
  let next_gamer = (List.hd (List.tl gamer_lst)).gamer in
  let play_result = Command.play gs gamer next_gamer phr pick in 
  match play_result with 
  | Legal gamestate -> 
    let turn_g = turn gamer gamer_lst phr in
    (print_tally gamestate (List.hd turn_g).gamer);
    recurse_command gamestate (List.hd turn_g).gamer 
      turn_g (winning gamestate gamer)
  | Illegal string -> print_endline string; 
    recurse_command gs gamer gamer_lst win

(** [c_uno gs gamer gamer_lst phr win] takes care of the situation in which 
    uno (defensive) has been called by [gamer]. If the move is Illegal, then 
    [gamer] is forced to draw 4 cards. Otherwise, [recurse_command] is called.
    Used by: [recurse_command] *)
and c_uno gs gamer gamer_lst phr win =
  let pick = 
    if gamer <> User then c_color_ai gs gamer_lst phr else
      (if phr = "Wild" || phr = "Wild +4" then pick_color "" else Any) in
  let next_gamer = (List.hd (List.tl gamer_lst)).gamer in
  let uno_result = Command.uno gs gamer next_gamer phr pick in 
  match uno_result with
  | Legal gamestate -> print_endline (uno_valid_print gamer);
    let turn_g = turn gamer gamer_lst phr in
    (print_tally gamestate (List.hd turn_g).gamer);
    recurse_command gamestate (List.hd turn_g).gamer turn_g win
  | Illegal string -> 
    if string = "nouno" then 
      (print_endline "\nYou did not call a valid uno. You've been penalized \
                      four cards.";
       let turn_g = turn gamer gamer_lst phr in 
       recurse_command (c_draw gs gamer 4) (List.hd turn_g).gamer turn_g win)
    else (print_endline string; recurse_command gs gamer gamer_lst win)

(** [c_color_ai gs gamer_lst phr] is the color that the next gamer has to play
    based on the card [phr] played by the AI. 
    Used by: [c_play], [c_uno] *)
and c_color_ai gs gamer_lst phr =
  print_endline (player_name (List.hd gamer_lst).gamer 
                 ^ " played a " ^ phr ^ ".");
  if phr = "Wild" || phr = "Wild +4" then 
    let curr_player_diff = (List.hd gamer_lst).diff in
    let curr_player = (List.hd gamer_lst).gamer in 
    match curr_player_diff with 
    | Easy -> Ai_easy.choose_color gs
    | Medium -> Ai_med.choose_color gs
    | Hard -> Ai_hard.choose_color gs curr_player
  else Any

(** [c_uno2 gs gamer called_on gamer_lst win] takes care of the situation 
    in which [gamer] calls uno (offensive) on another gamer [called_on]. 
    If the move is Illegal, then [gamer] is forced to draw 4 cards. Then the
    turn changes and [recurse command] is called again. 
    Used by: [recurse command] *)
and c_uno2 gs gamer called_on gamer_lst win =
  let uno2_result = Command.uno2 gs gamer called_on in 
  let turn_g = turn gamer gamer_lst "" in
  match uno2_result with 
  | Legal gamestate -> (print_endline (uno2_valid_print gamer called_on));
    recurse_command gamestate (List.hd turn_g).gamer turn_g win
  | Illegal string -> print_endline string;
    recurse_command (c_draw gs gamer 4) (List.hd turn_g).gamer turn_g win

(*------------ABOVE-------------recurse command-------------ABOVE-------------*)

(*------------BELOW-----------initialize uno game-----------BELOW-------------*)

(** [init_gamer_lst pl_lst] is an initialized record list of the gamers and 
    their respective difficulties, which is input by the user.
    Requires: [pl_lst] is an [ai_diff] list of length between 1 and 3
    Used by: [main ()] *)
let rec init_gamer_lst pl_lst = 
  let len = List.length pl_lst in 
  let user = {gamer = User; diff = Easy} in 
  let one_ai = {gamer = Player1; diff = List.hd (pl_lst)} in
  if len = 1 then user::one_ai::[]
  else if len = 2 then 
    let two_ai = {gamer = Player2; diff = List.hd (List.tl pl_lst)} in 
    user::one_ai::two_ai::[]
  else
    let two_ai = {gamer = Player2; diff = List.hd (List.tl pl_lst)} in 
    let three_ai = {gamer = Player3; diff = List.hd (List.rev pl_lst)} in 
    user::one_ai::two_ai::three_ai::[]

(** [assign_diff_to_ai nums ai_lst] is the [ai_diff] list corresponding to the
    user input numbers. 1 corresponds to Easy. 2 corresponds to Medium. 3
    corresponds to Hard. 
    Used by: *)
let rec assign_diff_to_ai nums ai_lst =
  match nums with 
  | [] -> ai_lst 
  | h::t -> 
    if h = 1 then (assign_diff_to_ai t (Easy::ai_lst))
    else if h = 2 then (assign_diff_to_ai t (Medium::ai_lst))
    else assign_diff_to_ai t (Hard::ai_lst)

(** [check_malform_ais str_lst] is true if [str_lst] only contains numbers of 
    values 1, 2, or 3.
    Requires: [str_lst] is a string list of length between 1 and 3 
    Used by: [prompt_init_ais ()] *)
let rec check_malformed_ais str_lst = 
  match str_lst with
  | [] -> true
  | h::t -> if (h = "1" || h = "2" || h = "3") then check_malformed_ais t 
    else false 

(** [prompt_init_ais ()] prompts the user for the number and difficulties
    of the AIs to play against, accounts for any mistakes from the user, and 
    then gives a list of length 1 to 3 of AI difficulties [ai_diff]. 
    Used by: [main ()] *)
let rec prompt_init_ais () =
  print_endline ("\nInput the number of AI(s) you would like to play against \
                  (up to 3) and their corresponding difficulties (1 = easy, \
                  2 = medium, 3 = hard). \nExamples: '3 3' = 2 AIs with hard \
                  difficulty, '2' = 1 AI with medium difficulty, '1 2' = 2 AIs \
                  with one easy and one medium. \nPlease enter your choice:\n");
  let difficulties = String.trim (read_line()) in 
  if difficulties = "" then prompt_init_ais ()
  else if difficulties = "quit" || difficulties = "Quit" 
  then (print_endline end_game; exit 0)
  else let str_lst = String.split_on_char ' ' difficulties in
    let str_lst_len = List.length str_lst in 
    if (str_lst_len > 0 && str_lst_len < 4 && check_malformed_ais str_lst) 
    then assign_diff_to_ai (List.map (int_of_string) str_lst) []
    else prompt_init_ais ()

(** [main ()] is the initial point of contact for the game to start. *)
let main () =
  print_endline ("\nWelcome to Uno! This game was coded by: Caroline Chu, \
                  Nishat Peuly, and Alice Zhao.");
  let ai_difficulties = prompt_init_ais () in 
  print_endline ("\nInput 'Rules' to review the rules. Input 'Commands' to \
                  learn the commands. Input 'Quit' to quit the game at any \
                  time. Have fun, and best of luck! \nThe game is starting \
                  now.......");
  let json_file = Yojson.Basic.from_file "init_small.json" in
  let num_ais = List.length ai_difficulties in
  let begin_uno = Gamestate.from_json json_file 7 num_ais in 
  recurse_command begin_uno User (init_gamer_lst ai_difficulties) false

(* Execute the game engine. *)
let () = main ()