open Gamestate
open Command
open Ai_med
open Ai_hard 
open Ai_easy 
open ANSITerminal

(** The type [ai_diff] represents the level of difficulties the user has chosen
    for the AI.*)
type ai_diff = Easy | Medium | Hard

(** The type [record] represents a record list of the gamers and their 
    corresponding difficulties. *)
type record = {gamer : Gamestate.gamer; diff : ai_diff}

(** [player_name gamer] is the string associated with the different AIs, which
    is used when printing messages to the user. *)
let player_name gamer =
  if gamer = Player1 then "Foster"
  else if gamer = Player2 then "Gries"
  else "White"

(*--------------BELOW-------------printing---------------BELOW----------------*)

(** [c_empty ()] prints when the command is empty. *)
let c_empty () =
  print_string[red] ("\nYour command was empty. If you are having trouble with \
                      the game, please refer to the rules with 'Rules' or the \
                      commands with 'Commands'.");
  print_string [cyan] ("\nPlease enter a valid command: \n")

(** [c_malformed ()] prints when the command is malformed. *)
let c_malformed () =
  print_string [red] ("\nYour command was malformed. If you are having trouble \
                       with the game, please refer to the rules with 'Rules' \
                       or the commands with 'Commands'.");
  print_string [cyan] ("\nPlease enter a valid command: \n")

(** [c_empty_color ()] prints when the color is empty. *)
let c_empty_color () =
  print_string [red] ("\nYour color was empty. If you are having trouble with \
                       the game, please refer to the rules with 'Rules' or the \
                       commands with 'Commands'.");
  print_string [cyan] ("\nPlease enter a valid color \
                        (Red, Yellow, Green, or Blue): \n")

(** [c_malformed_color ()] prints when the color is malformed. *)
let c_malformed_color () =
  print_string [red] ("\nYour color was malformed. If you are having trouble \
                       with the game, please refer to the rules with 'Rules' \
                       or the commands with 'Commands'.");
  print_string [cyan] ("\nPlease enter a valid \
                        color (Red, Yellow, Green, or Blue): \n")

(** [end_game ()] prints the ending message if the user quits before the game
    ends. *)
let end_game () =
  print_string [magenta] ("\nSorry to see you go.... Hope you liked the \
                           game! \nBest: Caroline, Nat, Alice :)\n")

(*--------------ABOVE-------------printing---------------ABOVE----------------*)

(*-----------BELOW----------draw, play, uno helpers-----------BELOW-----------*)

(** [player_drew gamer] prints when the AI has drawn a card. 
    Used by: [c_draw] *)
let player_drew gamer =
  if gamer = User then () else
    print_endline (player_name gamer ^ " drew card(s)."); 
  print_string [on_blue] "\n "
(** [winning gs gamer] is the boolean of whether the game has been won by 
    either the user or the AIs. If true, then ending messages are printed as
    well. 
    Used by: [c_play] *)
let winning gs gamer =
  let sec_str = " has won the game. \nYou should play again to redeem \
                 your honor.\n" in 
  if Gamestate.win_or_not gs gamer then
    if gamer = User then 
      ((print_string [magenta] ("\nYou have won the game! Congratulations and \
                                 thanks for playing! 
                                 \nBest: Alice, Caroline, Nat")); true)
    else ((print_string [magenta] ("\n" ^ player_name gamer ^ sec_str)); true)
  else false

(** [pick_color ()] is the color the user chooses after playing a wild card. 
    Used by: [c_play], [c_uno] *)
let pick_color () =
  (print_endline "\nYou played a wild card, which means you get to choose the \
                  \nnext color to be played.");              
  print_string [cyan] (" Please enter the next color (Red, Yellow, Green, \
                        Blue):\n");
  parse_color (read_line ())

(** [tally gs gamer] prints the tally against a player. Tally represents the
    number of cards the [gamer] has to draw if they cannot play a +2 or +4 card. 
    Used by: [c_play], [c_uno] *)
let tally gs gamer =
  let tally_num = string_of_int (Gamestate.current_tally_num gs) in 
  if tally_num = "0" then () else
    let curr_tally_g = Gamestate.current_tally_gamer gs in
    if curr_tally_g = User 
    then print_endline ("\nThe tally is now " ^ tally_num 
                        ^ " against you. If you don't have a +2 or +4 card, \
                           you should Draw.")
    else print_endline ("\nThe tally is now " ^ tally_num 
                        ^ " against " ^ player_name gamer ^ ".")

(** [valid_uno gamer] prints when a valid uno defensive has been 
    called by the [gamer].
    Used by: [uno] *)
let valid_uno gamer = 
  if gamer = User then 
    print_endline ("\nYou have called Uno successfully! One card away from \
                    winning!")
  else print_endline (player_name gamer ^ " has called uno! Looks like \
                                           you're gonna lose...unless...?");
  print_string [on_blue] "\n "

(** [invalid_uno ()] prints when the an invalid defensive uno was called. *)
let invalid_uno () =
  print_endline ("\nYou did not call a valid uno. You've been penalized \
                  four cards.")

(** [uno2_valid gamer called_on] is the string of when a valid uno offensive
    has been called on [called_on] by the [gamer]. 
    Used by: [uno2] *)
let uno2_valid gamer called_on =
  let uno_on_user = " called uno on you. You have been penalized 4 cards." in
  if gamer = User then 
    let first_str = "\nYou called uno on " in 
    let next_str = ". He has been forced to draw 4 cards." in 
    first_str ^ player_name called_on ^ next_str
  else player_name gamer ^ uno_on_user

(** [not_a_gamer ()] prints out a message of when the user calls uno offensive
    on a player not in the game. *)
let not_a_gamer () =
  print_endline ("\nYou did not call a valid offensive uno on a player that is \
                  in the game. Please try again.")

(*---------ABOVE---------draw, play, uno, uno2 helpers---------ABOVE----------*)

(*-----------BELOW----------recurse command helpers-----------BELOW-----------*)

(** [turn gamer gamer_lst phr] is the [record] list of the ordering of who
    plays next based on the [phr] that was just played. Takes care of the case
    when a Skip or Reverse card was played. 
    Used by: [recurse_command], [c_play], [c_uno], [c_uno2] *)
let turn gamer gamer_lst phr =
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

(** [player_plays gs gamer_lst] is the command given by the AI based on
    the difficulty of the AI. *)
let player_plays gs gamer_lst = 
  let difficulty = (List.hd gamer_lst).diff in 
  let the_gamer = (List.hd gamer_lst).gamer in
  if difficulty = Easy then Ai_easy.player_turn gs the_gamer
  else if difficulty = Medium then Ai_med.player_turn gs the_gamer
  else Ai_hard.player_turn gs the_gamer

(** [gamer_hand_size gs gamer_lst acc] is the string that gives the number 
    of cards for each of the gamers. *)
let rec gamer_hand_size gs gamer_lst acc =
  match gamer_lst with 
  | [] -> acc
  | h::t -> 
    let hand_size = string_of_int (Gamestate.hand_size gs h.gamer) in 
    if h.gamer = User 
    then gamer_hand_size gs t 
        (acc ^ "\n\nYou have " ^ hand_size ^ " card(s). ")
    else gamer_hand_size gs t 
        (acc ^ "\n" ^ player_name h.gamer ^ " has " ^ hand_size ^ " card(s). ")

(** [print_color_card str] prints out [str] based on which color it represents. 
    Requires: the first three letters of [str] represent color.
    Used by: [print_color_list] *)
let print_color_card str = 
  let lowercase_str = String.lowercase_ascii str in 
  let first_three = String.sub lowercase_str 0 3 in 
  if first_three = "red" then print_string [red] str
  else if first_three = "blu" then print_string [blue] str
  else if first_three = "gre" then print_string [green] str
  else if first_three = "yel" then print_string [yellow] str
  else print_string [default] str 

(** [print_color_list card_list last_card] prints out [card_list] according to
    the colors of the cards in it.
    Used by: [print_for_user] *)
let rec print_color_list card_list last_card = 
  match card_list with 
  |[] -> ()
  |h::t -> if h = last_card 
    then (print_color_card h; print_color_list t last_card)
    else (print_color_card h; print_string [default] ", "; 
          print_color_list t last_card)


(** [print_for_user gs gamer gamer_lst] prints necessary information for the 
    user such as the card on top of the discard pile, the color the user should
    match, and the user's hand. *)
let print_for_user gs gamer gamer_lst =
  (print_string [default] ("\n\nThe card on top of the discard pile is: ");
   Gamestate.last_card_played gs |> print_color_card);
  print_string [default] ("\nThe color you should match is: "); 
  (Gamestate.color_state gs) |> String.capitalize_ascii|> print_color_card; 
  print_string [default]("\nYour current hand has: ");
  let list = Gamestate.hand gs User in 
  let last_card = List.hd (List.rev list) in 
  print_color_list list last_card;
  let hand_to_str = gamer_hand_size gs gamer_lst "" in 
  print_endline (hand_to_str);
  print_endline ("\nIt is your turn to play!");
  print_string [cyan] ("\nEnter Your Command: \n")

(** [print_for_player gamer] prints when it is the AI's turn to play. *)
let print_for_player gamer =
  (print_endline ("\n\n" ^player_name gamer ^ " is playing. Wait a second..."));
  (Unix.sleep(1))

(*-----------ABOVE----------recurse command helpers-----------ABOVE-----------*)

(*--------------BELOW--------------parsing---------------BELOW----------------*)

(** [parse_command str] is the command the user inputs. *)
let rec parse_command str = 
  match (Command.parse str) with 
  | command -> command
  | exception Command.Empty -> c_empty (); 
    parse_command (read_line ())
  | exception Command.Malformed -> c_malformed (); 
    parse_command (read_line ())

(** [parse_color str] is the color the user's selection after playing a wild 
    card. *)
let rec parse_color str =
  match (Command.parse_color str) with 
  | color -> color
  | exception Command.Empty -> c_empty_color (); parse_color (read_line ())
  | exception Command.Malformed -> c_malformed_color (); 
    parse_color (read_line ())

(*--------------ABOVE--------------parsing---------------ABOVE----------------*)

(*------------BELOW-----------recurse command--------------BELOW--------------*)

(** [recurse_command gs gamer gamer_lst win] is the recursive state that 
    handles the commands the user inputs. This is the main entry function for 
    commands. *)
let rec recurse_command gs gamer gamer_lst win =
  let _ = if win then exit 0 else
    if gamer = User then print_for_user gs gamer gamer_lst
    else print_for_player gamer in
  let command_from_gamer = 
    if gamer <> User then player_plays gs gamer_lst
    else (parse_command (read_line ())) in
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
  | Command.Quit -> end_game (); exit 0

(** [c_draw gs gamer num] is a new Gamestate in which the [gamer] has drawn or
    been forced to draw [num] cards. *)
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
      (if phr = "Wild" || phr = "Wild +4" then pick_color () else Any) in
  let next_gamer = (List.hd (List.tl gamer_lst)).gamer in
  let play_result = Command.play gs gamer next_gamer phr pick in 
  match play_result with 
  | Legal gamestate -> 
    let turn_g = turn gamer gamer_lst phr in
    (tally gamestate (List.hd turn_g).gamer);
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
      (if phr = "Wild" || phr = "Wild +4" then pick_color () else Any) in
  let next_gamer = (List.hd (List.tl gamer_lst)).gamer in
  let uno_result = Command.uno gs gamer next_gamer phr pick in 
  let turn_g = turn gamer gamer_lst phr in
  match uno_result with
  | Legal gamestate -> (valid_uno gamer);
    (tally gamestate (List.hd turn_g).gamer);
    recurse_command gamestate (List.hd turn_g).gamer turn_g win
  | Illegal string -> 
    if string = "nouno" then 
      (invalid_uno (); 
       recurse_command (c_draw gs gamer 4) (List.hd turn_g).gamer turn_g win)
    else (print_endline string; recurse_command gs gamer gamer_lst win)

(** [c_color_ai gs gamer_lst phr] is the color that the next gamer has to play
    based on the card [phr] played by the AI. 
    Used by: [c_play], [c_uno] *)
and c_color_ai gs gamer_lst phr =
  print_string [default] (player_name (List.hd gamer_lst).gamer 
                          ^ " played a "); 
  print_color_card phr; 
  print_string [default] ".\n" ;
  Unix.sleep(1);
  print_string [on_blue] "\n ";
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
  | Legal gamestate -> (print_endline (uno2_valid gamer called_on));
    recurse_command gamestate (List.hd turn_g).gamer turn_g win
  | Illegal string -> if string = "notagamer" then 
      (not_a_gamer (); recurse_command gs gamer gamer_lst win)
    else recurse_command (c_draw gs gamer 4) (List.hd turn_g).gamer turn_g win

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
  print_string [magenta] ("\nTo start us off... ");
  print_endline ("\nInput the number of AI(s) you would like to play against \
                  (up to 3) and their corresponding difficulties (1 = easy, \
                  2 = medium, 3 = hard). \nExamples: '3 3' = 2 AIs with hard \
                  difficulty, '2' = 1 AI with medium difficulty, '1 2' = 2 AIs \
                  with one easy and one medium.");
  print_string [cyan] ("\nPlease enter your choice:\n");
  let difficulties = String.trim (read_line()) in 
  if difficulties = "" then prompt_init_ais ()
  else if difficulties = "quit" || difficulties = "Quit" 
  then (end_game(); exit 0)
  else let str_lst = String.split_on_char ' ' difficulties in
    let str_lst_len = List.length str_lst in 
    if (str_lst_len > 0 && str_lst_len < 4 && check_malformed_ais str_lst) 
    then assign_diff_to_ai (List.map (int_of_string) str_lst) []
    else prompt_init_ais ()

(**[print_opponent rec_lst] prints the names of the AIs the user is going to 
   play the game with. *)
let print_opponent rec_lst = 
  let rec print_names rec_lst names_str = 
    match rec_lst with 
    |[] -> names_str
    |h::t -> if names_str <> "" then 
        print_names t (names_str ^ ", " ^ (player_name h.gamer)) 
      else print_names t (names_str ^ (player_name h.gamer))
  in 
  let to_be_printed = ("\nYou will be playing this game against our AI(s): \
                       " ^ (print_names (List.tl rec_lst) "") ^ "\n") in 
  (ANSITerminal.print_string [red] to_be_printed)

(** [main ()] is the initial point of contact for the game to start. *)
let main () =
  print_string [magenta;Bold] ("\nWelcome to Uno!");  
  print_string [magenta] ("\n\nCoded by: Caroline Chu, \
                           Nishat Peuly, and Alice Zhao.\n\n");
  let ai_difficulties = prompt_init_ais () in 
  let record_lst = init_gamer_lst ai_difficulties in 
  print_opponent record_lst;
  print_endline ("\nInput 'Rules' to review the rules. \nInput 'Commands' to \
                  learn the commands. \nInput 'Quit' to quit the game at any \
                  time. \nHave fun, and best of luck! The game is starting \
                  now......."); Unix.sleep(1);
  let json_file = Yojson.Basic.from_file "init2.json" in
  let num_ais = List.length ai_difficulties in
  let begin_uno = Gamestate.from_json json_file 7 num_ais in 
  recurse_command begin_uno User (record_lst) false

(*------------ABOVE-----------initialize uno game-----------ABOVE-------------*)

(* Execute the game engine. *)
let () = main ()