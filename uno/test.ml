open OUnit2
open Gamestate
open Command

(* -4: The test plan is missing.
   -1: The test plan does not explain which parts of the system were automatically
   tested by OUnit vs. manually tested.
   -1: The test plan does not explain what modules were tested by OUnit and how 
   test cases were developed (black box, glass box, randomized, etc.).
   -1: The test plan does not provide an argument for why the testing approach 
   demonstrates the correctness of the system.*)

(*TEST PLAN: 
  Because we implemented different modules for this game: 
  (Nishat -> gamestate)
   Alice -> command 
   Caroline -> the ai modules)
   We tested them separately, and we had different approaches to testing the 
   modules and their functions. Main was testing entirely by manual testing.

   As this project was split into two sprints, we made a lot of structural 
   changes in the second sprint that needed to be tested. So we kept all of our 
   first sprint tests and just wrote more test-cases for the second sprint 
   instead of integrating them to save time.  
   ----------------------------------------------------------------------------
   Nishat: 

   I used white-box testing for the functions in gamestate.mli that return an 
   altered game state: draw, play, uno-defensive, uno-offensive. I chose this 
   approach because in our game, these functions are only called after the 
   command from the user has been parsed and verified, and in the AI modules 
   which have been programmed to never make invalid calls. 
   So the set of invalid calls to these functions that could be made in the game 
   is predictable, and we can get away with testing that the right exceptions 
   are thrown in those cases. 

   I used white-box testing for the initialization function from_json by 
   creating a from_json_unshuffled, and using the functions that return game 
   state attributes to check that from_json dealt cards in the correct order, 
   assigned the correct correct lists to the fields of type t. I could not test 
   functions such as last_card_played and hand and hand_size independently of 
   from_json, but since they are simple functions that retrieve information 
   about gamestate.t, I assumed they worked correctly. 

   ----------------------------------------------------------------------------
   Alice: 

   ---------------------------------------------------------------------------- 
   Caroline: 

   All three AI modules have only two visible functions - player_turn and 
   choose_color - that return the same types of outputs, so I could use the same 
   method of white-box testing for them all. That is, I would create particular
   gamestates in which each gamer's hand is specifically tailored to force 
   certain situations (eg. not having a playable card and having to draw, or 
   having only one playable card), and test whether the AI returned the expected 
   prioritized action. The AI modules depend heavily on Gamestate and Command 
   functions, so those modules were tested and made certain to be correct before 
   these were tested so I could ensure their proper functionality.
   
*)


(*---------------------------GAMESTATE TESTS----------------------------------*)



let json_uno = Yojson.Basic.from_file "init.json"
let gs_7 = from_json_unshuffled json_uno 7 1 

let json_test_1 = Yojson.Basic.from_file "test_deck_1.json"


(* FIRST SPRINT: only simple cards *)

(* User and Player1 start out with 1 card in hand each *)
let gs_1 = from_json_unshuffled json_test_1 1 1 

let gs_11 = Gamestate.draw gs_1 User 1 
let gs_12 = Gamestate.draw gs_1 Player1 1
let gs_13 = Gamestate.draw gs_1 User 2 
let gs_14 = Gamestate.draw gs_13 Player1 1 

(* User and Player1 each call uno_defensive *)
let gs_15 = Gamestate.uno_defensive gs_1 User 
let gs_16 = Gamestate.uno_defensive gs_15 Player1

(* User and Player1 each call uno_offensive *)
let gs_17 = Gamestate.uno_offensive gs_1 User Player1 
let gs_18 = Gamestate.uno_offensive gs_1 Player1 User 

(* User and Player1 each win *)
let gs_19 = Gamestate.play gs_15 User Player1"Red 0" ""
let gs_20 = Gamestate.play gs_1 Player1 User "Red 1" ""

(* User and Player1 start out with 2 cards in hand each *)
let gs_2 = from_json_unshuffled json_test_1 2 1 

let gs_21 = Gamestate.play gs_2 User Player1 "Red 0" ""
let gs_22 = Gamestate.play gs_21 Player1 User "Red 2" ""

(* draw pile is empty. discard pile is shuffled*)
let gs_23 = Gamestate.draw gs_22 User 2 
let gs_24 = Gamestate.draw gs_23 Player1 1

(*---------------------------------------------------------------------------*)
(* SECOND SPRINT: ACTION CARDS *)

(*testing that wild works*)
let json_test_6 = Yojson.Basic.from_file "test_deck_6.json" 
let gs_3 = from_json_unshuffled json_test_6 2 1

let gs_31 = Gamestate.play gs_3 User Player1 "Wild" "red"
let gs_32 = Gamestate.play gs_31 Player1 User "Wild" "blue"

let gs_33 = Gamestate.draw gs_32 User 1
let gs_331 = Gamestate.draw gs_33 Player1 1
let gs_34 = Gamestate.play gs_331 User Player1 "Blue +2" ""
let gs_341 = Gamestate.play gs_34 Player1 User "Red +2" ""

let gs_35 = Gamestate.draw gs_34 Player1 1

(*testing that wild +4 works*)
let json_test_7 = Yojson.Basic.from_file "test_deck_7.json"
let gs_4 = from_json_unshuffled json_test_7 2 1

let gs_41 = Gamestate.play gs_4 User Player1 "Wild +4" "red"
let gs_42 = Gamestate.play gs_41 Player1 User "Wild +4" "blue"

let gs_411 = Gamestate.draw gs_41 Player1 2


(*Helper functions to test exceptions*)
let exn_test_1 gs g ng card_name =
  (try gs_1 = (Gamestate.play gs g ng card_name "") 
   with (Gamestate.CardNotInHand card)-> (card = card_name))

let exn_test_2 gs g ng card_name color_str = 
  try gs_1 = (Gamestate.play gs g ng card_name color_str)
  with (Gamestate.MisMatch card)-> (card = card_name)

let exn_test_3 gs g = 
  try gs_1 = (Gamestate.uno_defensive gs g) 
  with (Gamestate.Nouno g)-> true

let exn_test_4 gs g1 g2 = 
  try gs_1 = (Gamestate.uno_offensive gs g1 g2) 
  with (Gamestate.Nouno g) -> (g = g2) 

let exn_test_5 gs g ng card_name color_str = 
  try gs_1 = (Gamestate.play gs g ng card_name color_str)
  with (Gamestate.TallyIllegal) -> true

let gamestate_tests =
  [
    (* testing from_json *)
    (*.......................................................................*)
    (* Is the correct no. of cards dealt to the gamers? *)
    (* Assuming that hand_size works correctly *)
    "from_json_1" >:: (fun _ -> assert_equal 1 (hand_size gs_1 User));
    "from_json_2" >:: (fun _ -> assert_equal 1 (hand_size gs_1 Player1));

    (* Is the correct order maintained when dealing cards? *)
    (* Assuming hand works correctly *)
    "from_json_3" >:: (fun _ -> assert_equal ["Red 0"] (hand gs_1 User));
    "from_json_4" >:: (fun _ -> assert_equal ["Red 1"] (hand gs_1 Player1));  
    "from_json_5" >:: (fun _ -> assert_equal 
                          ["Red 1"; "Red 0"] (hand gs_2 User));
    "from_json_6" >:: (fun _ -> assert_equal 
                          ["Red 3"; "Red 2"] (hand gs_2 Player1));

    (* Is the correct card in the discard pile? *)
    "from_json_7" >:: (fun _ -> assert_equal "Red 2" (last_card_played gs_1));
    "from_json_8" >:: (fun _ -> assert_equal "Red 4" (last_card_played gs_2));


    (* testing number_search *)
    (*.......................................................................*)

    "ns_test1" >:: (fun _-> assert_equal 1 (number_search gs_1 Player1 "Red 1"));
    "ns_test2" >:: (fun _-> assert_equal 3 (number_search gs_2 Player1 "Red 3"));


    (* testing color_search *)
    (*.......................................................................*)

    "cs_test1">::(fun _ -> assert_equal "red" (color_search gs_1 User "Red 0"));
    "cs_test2">::(fun _ -> assert_equal "red" 
                     (color_search gs_2 Player1 "Red 2"));


    (* testing draw *)
    (*.......................................................................*)

    "draw_test_1" >:: (fun _ -> assert_equal 
                          ["Red 3";"Red 0"] (hand gs_11 User)); 
    "draw_test_2" >:: (fun _ -> assert_equal 
                          ["Red 3";"Red 1"] (hand gs_12 Player1)); 
    "draw_test_3" >:: (fun _ -> assert_equal
                          ["Red 4"; "Red 3"; "Red 0"] (hand gs_13 User));
    "draw_test_4" >:: (fun _ -> assert_equal 
                          ["Red 5"; "Red 1"] (hand gs_14 Player1));
    "draw_test_5" >:: (fun _ -> assert_equal 
                          ["Red 6"; "Red 5"; "Red 1"] (hand gs_23 User));


    (* testing play *)
    (*.......................................................................*)

    "play_test_1" >:: (fun _ -> assert_equal ["Red 1"] (hand gs_21 User));
    "play_test_2" >:: (fun _ -> assert_equal "Red 0" (last_card_played gs_21));

    "play_test_3" >:: (fun _ -> assert_equal ["Red 3"] (hand gs_22 Player1));
    "play_test_4" >:: (fun _ -> assert_equal "Red 2" (last_card_played gs_22));

    (*Find a way to test the right exceptions are thrown *)
    "play_test_5" >:: (fun _ -> assert_equal true 
                          (exn_test_1  gs_2 User Player1 "Green 0"));
    "play_test_5" >:: (fun _ -> assert_equal true
                          (exn_test_1  gs_2 Player1 User "Red 00"));  

    (* testing uno_defensive*) 
    (*.......................................................................*)

    "uno_d_test_1" >:: (fun _ -> assert_equal true (uno_state gs_15 User));
    "uno_d_test_2" >:: (fun _ -> assert_equal false (uno_state gs_15 Player1));
    "uno_d_test_3" >:: (fun _ -> assert_equal true (uno_state gs_16 Player1));
    "uno_d_test_4" >:: (fun _ -> assert_equal true (exn_test_3 gs_2 User)); 


    (* testing uno_offensive *)   
    (*.......................................................................*)

    "uno_o_test_1" >:: (fun _ -> assert_equal 5 (hand_size gs_17 Player1)); 
    "uno_o_test_2" >:: (fun _ -> assert_equal 5 (hand_size gs_18 User));
    "uno_o_test_3" >:: (fun _ -> assert_equal true 
                           (exn_test_4 gs_16 User Player1));
    "uno_o_test_4" >:: (fun _ -> assert_equal true 
                           (exn_test_4 gs_2 User Player1));

    (* testing win_or_not *)
    (*.......................................................................*)

    "win_test_1" >:: (fun _ -> assert_equal true (win_or_not gs_19 User)); 
    "win_test_2" >:: (fun _ -> assert_equal false (win_or_not gs_20 Player1));

    (* testing after adding action cards into the deck *)

    (* testing that adding wild works: changes color*)
    "wild_test_1" >:: (fun _ -> assert_equal "red" (color_state gs_31));
    "wild_test_2" >:: (fun _ -> assert_equal "black" 
                          (last_card_played_color gs_31));
    "wild_test_3" >:: (fun _ -> assert_equal 0 (current_tally_num gs_31));
    "wild_test_31" >:: (fun _ -> assert_equal true 
                           (exn_test_2 gs_31 Player1 User "Blue 0" ""));
    "wild_test_4" >:: (fun _ -> assert_equal "blue" (color_state gs_32)); 
    "wild_test_5" >:: (fun _ -> assert_equal 13 (last_card_played_number gs_32)); 
    "wild_test_51" >::(fun _ -> assert_equal true 
                          (exn_test_2 gs_32 User Player1 "Red 0" ""));

    (* testing +2*)
    "+2_test_1" >::(fun _ -> assert_equal 2 (current_tally_num gs_34)); 
    "+2_test_2" >::(fun _ -> assert_equal Player1 (current_tally_gamer gs_34));
    "+2_test_3" >::(fun _ -> assert_equal true 
                       (exn_test_5 gs_34 Player1 User "Blue 0" ""));

    "+2_test_4" >:: (fun _ -> assert_equal 4 (hand_size gs_35 Player1));
    "+2_test_5" >:: (fun _ -> assert_equal 4 (current_tally_num gs_341));
    "+2_test_6" >:: (fun _ -> assert_equal User (current_tally_gamer gs_341));

    (* testing Wild +4 *)
    "+4_test_1" >:: (fun _ -> assert_equal 4 (current_tally_num gs_41));
    "+4_test_2" >:: (fun _ -> assert_equal Player1 (current_tally_gamer gs_41));
    "+4_test_3" >:: (fun _ -> assert_equal true 
                        (exn_test_5 gs_41 Player1 User "Red +2" ""));
    "+4_test_4" >:: (fun _ -> assert_equal 8 (current_tally_num gs_42));
    "+4_test_5" >:: (fun _ -> assert_equal User (current_tally_gamer gs_42));
    "+4_test_6" >:: (fun _ -> assert_equal 6 (hand_size gs_411 Player1));



  ]


(*.........................testing command below..............................*)

(* ------BELOW------for first sprint, using non-action cards------BELOW-------*)
(*
  let json_com = Yojson.Basic.from_file "test_deck_command.json"

  let gs_c = from_json_unshuffled json_com 1
  (* for draw *)
  let gs_c1 = Gamestate.draw gs_c User 1
  let gs_c2 = Gamestate.draw gs_c Player 1
  (* for play *)
  let gs_c3 = Gamestate.play gs_c1 User "Red 0" ""
  let gs_c4 = Gamestate.play gs_c2 Player "Red 3" ""
  let gs_c5 = Gamestate.draw gs_c1 User 1
  (* for uno *)
  let gs_c6 = Gamestate.draw gs_c2 Player 1
  let gs_c7 = Gamestate.uno_defensive gs_c3 User
  let gs_c8 = Gamestate.uno_defensive gs_c4 Player
  (* for uno2 *)
  let gs_c9 = Gamestate.uno_offensive gs_c1 User Player
  let gs_c10 = Gamestate.uno_offensive gs_c2 Player User
  let uno2_il = "\nYou did not call a valid offensive uno. The other player does \
               not have Uno. You have been forced to draw 4 cards."

  let command_tests_sprint_1 =
  [
    (* test out parse *)
    "pars_draw" >:: (fun _ -> assert_equal Draw (Command.parse " drAw "));
    "pars_play" >:: (fun _ -> assert_equal (Play "Red 2") 
                        (Command.parse "PlaY Red 2"));
    "pars_uno" >:: (fun _ -> assert_equal (Uno "Blue 0") 
                       (Command.parse "uNo bLue 0"));
    "pars_uno2" >:: (fun _ -> assert_equal (Uno2 User) 
                        (Command.parse "uno2 User"));
    "pars_rules" >:: (fun _ -> assert_equal Rules (Command.parse "    ruLes"));
    "pars_commands" >:: (fun _ -> assert_equal Commands 
                            (Command.parse "COmmands "));
    "pars_empty" >:: (fun _ -> assert_raises 
                         Empty (fun () -> Command.parse "  "));
    "pars_malf" >:: (fun _ -> assert_raises 
                        Malformed (fun () -> Command.parse "blaH"));
    (* ---------------------------------------------------------------------- *)
    (* test out draw *)
    "draw_user" >:: (fun _ -> assert_equal 
                        (Legal gs_c1) (Command.draw gs_c User 1));
    "draw_player" >:: (fun _ -> assert_equal 
                          (Legal gs_c2) (Command.draw gs_c Player 1));
    (* ---------------------------------------------------------------------- *)
    (* test out play *)
    "play_user_legal" >:: (fun _ -> assert_equal (Legal gs_c3) 
                              (Command.play gs_c1 User "Red 0" Any ));
    "play_player_legal" >:: (fun _ -> assert_equal
                                (Legal gs_c4) 
                                (Command.play gs_c2 Player "Red 3" Any));
    "play_not_in_hand" >:: (fun _ -> assert_equal
                               (Illegal "\nYou tried to play a card not in \
                                         your hand: Red 4")
                               (Command.play gs_c1 User "Red 4" Any));
    "play_mismatch" >:: (fun _ -> assert_equal
                            (Illegal "\nYour card does not match the card last \
                                      played: Blue 0")
                            (Command.play gs_c5 User "Blue 0" Any));
    (* ---------------------------------------------------------------------- *)
    (* test out uno *)
    "uno_not_in_hand" >:: (fun _ -> assert_equal
                              (Illegal "\nYou tried to play a card not in your \
                                        hand: Red 4")
                              (Command.uno gs_c1 User "Red 4" Any));
    "uno_mismatch" >:: (fun _ -> assert_equal
                           (Illegal "\nYour card does not match the card last \
                                     played: Blue 0")
                           (Command.uno gs_c5 User "Blue 0" Any));
    "uno_user_illegal" >:: (fun _ -> assert_equal
                               (Illegal "nouno") 
                               (Command.uno gs_c5 User "Red 3" Any));
    "uno_user_legal" >:: (fun _ -> assert_equal (Legal gs_c7) 
                             (Command.uno gs_c1 User "Red 0" Any));
    "uno_player_illegal" >:: (fun _ -> assert_equal (Illegal "nouno") 
                                 (Command.uno gs_c6 Player "Red 1" Any));
    "uno_player_legal" >:: (fun _ -> assert_equal (Legal gs_c8) 
                               (Command.uno gs_c2 Player "Red 3" Any));
    (* ---------------------------------------------------------------------- *)
    (* test out uno2 *)
    "uno2_user_illegal" >:: (fun _ -> assert_equal
                                (Illegal uno2_il) 
                                (Command.uno2 gs_c2 User Player));
    "uno2_user_legal" >:: (fun _ -> assert_equal
                              (Legal gs_c9) (Command.uno2 gs_c1 User Player));
    "uno2_player_illegal" >:: (fun _ -> assert_equal
                                  (Illegal uno2_il) 
                                  (Command.uno2 gs_c1 Player User));
    "uno2_player_legal" >:: (fun _ -> assert_equal
                                (Legal gs_c10) 
                                (Command.uno2 gs_c2 Player User));

  ]
  (* ------ABOVE------for first sprint, using non-action cards------ABOVE-------*)

  (* ------BELOW-------for second sprint, using action cards-------BELOW--------*)

  let json_com2 = Yojson.Basic.from_file "test_deck_command_2.json"

  let gs_cc1 = from_json_unshuffled json_com2 1
  let gs_cc2 = Gamestate.draw gs_cc1 User 1
  let gs_cc3 = Gamestate.play gs_cc2 User "Wild +4" "red"
  let gs_cc4 = Gamestate.uno_defensive gs_cc3 User
  let gs_cc5 = Gamestate.draw gs_cc3 User 2
  let gs_cc6 = Gamestate.play gs_cc5 User "Red +2" ""

  let command_tests_sprint_2 =
  [
    (* test out parse_color *)
    "parsc_red" >:: (fun _ -> assert_equal Red 
                        (Command.parse_color "Red"));
    "parsc_yellow" >:: (fun _ -> assert_equal Yellow 
                           (Command.parse_color "YelLow"));
    "parsc_green" >:: (fun _ -> assert_equal Green 
                          (Command.parse_color "  grEen  "));
    "parsc_blue" >:: (fun _ -> assert_equal Blue 
                         (Command.parse_color "bLue"));
    "parsc_empty" >:: (fun _ -> assert_raises Empty 
                          (fun () -> (Command.parse_color "         ")));
    "parsc_malf" >:: (fun _ -> assert_raises Malformed 
                         (fun () -> (Command.parse_color "  purplE")));
    (* test out play *)
    "play_+4" >:: (fun _ -> assert_equal (Legal gs_cc3) 
                      (Command.play gs_cc2 User "Wild +4" Red));
    "play_ill_+4" >:: (fun _ -> assert_equal 
                          (Illegal ("\nThe last card played was a +4, which \
                                     means you must draw cards if you do \
                                     not have a +4 card.")) 
                          (Command.play gs_cc3 Player "Red 0" Any));
    "play_ill_+2" >:: (fun _ -> assert_equal 
                          (Illegal ("\nThe last card played was a +2, which \
                                     means you must draw cards if you do \
                                     not have a +2 or +4 card to play.")) 
                          (Command.play gs_cc6 Player "Red 0" Any));
    (* test out uno *)
    "uno_+4" >:: (fun _ -> assert_equal (Legal gs_cc4) 
                     (Command.uno gs_cc2 User "Wild +4" Red));
  ]
  (* ------ABOVE-------for second sprint, using action cards-------ABOVE--------*)

  (*.........................testing command above..............................*)
*)


(*.........................testing AIs below..................................*)

  open Ai_med

  (* Making gamestates to test player: *)

  (* gs_21 is where User has 1 card and Player has 2, Red 0 is last card played 
   (from above) *)

  (* gs_102 is where both gamers have 2 cards and Red 0 is the last card played *)
  let gs_101 = Gamestate.draw gs_2 User 1
  let gs_102 = Gamestate.play gs_101 User Player1 "Red 0" ""

  (* gs_113 is where User has 2 cards, Player has 3, and Red 0 is the last played *)
  let gs_111 = Gamestate.draw gs_2 User 1 
  let gs_112 = Gamestate.draw gs_111 Player1 1 
  let gs_113 = Gamestate.play gs_112 User Player1 "Red 0" ""

  (* In json_test_2, some of the cards are blue instead of red *)
  let json_test_2 = Yojson.Basic.from_file "test_deck_2.json"

  (* gs_203 is where User has 2 red cards, Player has 3 blue, Red 0 is the last 
   card played *)
  let gs_200 = from_json_unshuffled json_test_2 2 1
  let gs_201 = Gamestate.draw gs_200 User 1 
  let gs_202 = Gamestate.draw gs_201 Player1 1 
  let gs_203 = Gamestate.play gs_202 User Player1 "Red 0" ""

  (* gs_204 is where User has 2 red cards, Player has 3 blue+1 red, Red 0 is the 
   last card played *)
  let gs_204 = Gamestate.draw gs_203 Player1 1 

  (* gs_205 is where User has 2 red cards, Player has 2 blue, Red 0 is the last 
   card played *)
  let gs_205 = Gamestate.play gs_201 User Player1 "Red 0" ""

  (* gs_304 is where User has 2 red cards, Player has 2 blue, Blue 0 is the last 
   card played *)
  let json_test_3 = Yojson.Basic.from_file "test_deck_3.json"
  let gs_300 = from_json_unshuffled json_test_3 2 1
  let gs_301 = Gamestate.draw gs_300 User 1 
  let gs_302 = Gamestate.draw gs_301 User 1 
  let gs_303 = Gamestate.play gs_302 User Player1 "Red 0" ""
  let gs_304 = Gamestate.play gs_303 User Player1 "Blue 0" ""


  (** [make_ai_tests_sprint_1 name t expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [ai_tests_sprint_1]. *)
  let make_ai_tests_sprint_1
    (name : string) 
    (t)
    (expected_output) : test =
  name >:: (fun _ -> assert_equal (Ai_med.player_turn t Player1) expected_output )

  let ai_tests_sprint_1 =
  [
    (* testing player_turn *)
    make_ai_tests_sprint_1 "uno2" gs_21 (Uno2 User);
    make_ai_tests_sprint_1 "uno with one playable" gs_102 (Uno "Red 3");
    make_ai_tests_sprint_1 "uno with multiple playable" gs_304 (Uno "Blue 3");
    make_ai_tests_sprint_1 "play with multiple playable" gs_113 (Play "Red 6");
    make_ai_tests_sprint_1 "play with one playable" gs_203 (Play "Red 6");
    make_ai_tests_sprint_1 "draw" gs_200 (Draw);
    make_ai_tests_sprint_1 "draw despite uno" gs_205 (Draw);
  ]


  open Ai_hard

  (* Making gamestates to test player2: *)

  (* gs_2 is where User has 2 cards, Player has 2 (0 action), Red 4 is the last 
   card played (from above) *)

  (* In json_test_4, some action cards are included *)
  let json_test_4 = Yojson.Basic.from_file "test_deck_4.json"

  (* gs_401 is where User has 2 cards, Player has 3 (1 action), Red +2 is the last 
   card played *)
  let gs_400 = from_json_unshuffled json_test_4 3 1
  let gs_401 = Gamestate.play gs_400 User Player1 "Red +2" ""

  (* gs_404 is where User has 4 cards, Player has 4 (1 action), Wild +4 is the 
   last card played *)
  let gs_402 = Gamestate.draw gs_400 User 1 
  let gs_403 = Gamestate.draw gs_402 Player1 1 
  let gs_404 = Gamestate.play gs_403 User Player1 "Wild +4" "Red"

  (* gs_405 is where User has 3 cards, Player has 4 (2 action), Red 2 is the last 
   card played *)
  let gs_405 = Gamestate.draw gs_400 Player1 1 

  (* gs_406 is where User has 4 cards, Player has 4 (2 action), Red 2 is the last 
   card played *)
  let gs_406 = Gamestate.draw gs_405 User 1 

  (* gs_408 is where User has 4 cards, Player has 2 (2 action), Red 4 is the last 
   card played *)
  let gs_407 = Gamestate.play gs_406 Player1 User "Red 3" ""
  let gs_408 = Gamestate.play gs_407 Player1 User "Red 4" ""


  (* In json_test_5, some action cards plus different colors are included *)
  let json_test_5 = Yojson.Basic.from_file "test_deck_5.json"

  (* gs_501 is where User has 2 cards, Player has 6 (2 action, 2 blue), Red 1 is 
   the last card played *)
  let gs_500 = from_json_unshuffled json_test_5 2 1
  let gs_501 = Gamestate.draw gs_500 Player1 4

  (* gs_502 is where User has 1 cards, Player has 7 (1 action, 3 green), Red 1 is 
   the last card played *)
  let gs_502 = Gamestate.draw gs_501 Player1 2

  (* gs_503 is where User has 1 cards, Player has 10 (1 action, 4 yellow), Red 1 
   is the last card played *)
  let gs_503 = Gamestate.draw gs_502 Player1 3

  (* gs_800 and above are gamestates to test multiple AIs using test deck 8 *)
  let json_test_8 = Yojson.Basic.from_file "test_deck_8.json"
  let gs_800 = from_json_unshuffled json_test_8 2 2
  let gs_801 = Gamestate.play gs_800 Player2 User "Blue 3" ""
  let gs_802 = Gamestate.draw gs_801 Player2 2
  let gs_803 = Gamestate.play gs_802 User Player1 "Red 3" ""
  let gs_804 = Gamestate.play gs_803 Player1 Player2 "Green 3" ""
  let gs_805 = Gamestate.draw gs_804 User 2
  let gs_806 = Gamestate.draw gs_805 Player2 1
  let gs_807 = Gamestate.play gs_806 Player2 Player2 "Green 1" ""


  (** [make_ai_tests_sprint_2 name t expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [ai_tests_sprint_2]. *)
  let make_ai_tests_sprint_2
    (name : string) 
    (t)
    (expected_output) : test =
  name >:: (fun _ -> assert_equal (Ai_hard.player_turn t Player1) expected_output)

    (** [make_ais_tests_sprint_2 name t expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [ais_tests_sprint_2]. *)
  let make_ais_tests_sprint_2
    (name : string) 
    (t)
    (expected_output) : test =
  name >:: (fun _ -> assert_equal (Ai_hard.player_turn t Player2) expected_output)

  let ai_tests_sprint_2 =
  [
    make_ai_tests_sprint_2 "play +2 against +2" gs_401 (Play "Red +2");
    make_ai_tests_sprint_2 "play +4 against +4" gs_404 (Play "Wild +4");
    make_ai_tests_sprint_2 "play action card when user hand <4" gs_405 
      (Play "Wild +4");
    make_ai_tests_sprint_2 "play norm card when user hand >=4" gs_406 
      (Play "Red 4");
    make_ai_tests_sprint_2 "play norm card when u.h. <4 w/o choice" gs_2 
      (Uno "Red 3");
    make_ai_tests_sprint_2 "play action card when u.h. >=4 w/o choice" 
      gs_408 (Uno "Red +2");
    make_ai_tests_sprint_2 "play wild card with most color blue" 
      gs_501 (Play "Wild +4");
    make_ai_tests_sprint_2 "play wild card with most color green" 
      gs_502 (Play "Wild +4");
    make_ai_tests_sprint_2 "play wild card with most color yellow" 
      gs_503 (Play "Wild +4");

    make_ais_tests_sprint_2 "player2 unos" gs_800 (Uno "Blue 3");
    make_ais_tests_sprint_2 "player2 draws" gs_801 (Draw);
    make_ais_tests_sprint_2 "player2 uno2s user" gs_804 (Uno2 User);
    make_ais_tests_sprint_2 "player2 plays after player1" gs_805 (Play "Green 1");
    make_ais_tests_sprint_2 "player2 plays wild" gs_807 (Play "Wild");
  ]

(*.........................testing AIs above..................................*)


let suite =
  "test suite for uno_game"  >::: List.flatten [
    gamestate_tests;
    (*command_tests_sprint_1;
      command_tests_sprint_2;*)
      ai_tests_sprint_1;
      ai_tests_sprint_2;
  ]

let _ = run_test_tt_main suite