open OUnit2
open OUnitTest
open Gamestate
open Command

let json_uno = Yojson.Basic.from_file "init.json"
let gs_7 = from_json_unshuffled json_uno 7 

let json_test = Yojson.Basic.from_file "test_deck_1.json"


(* User and Player start out with 1 card in hand each *)
let gs_1 = from_json_unshuffled json_test 1

let gs_11 = Gamestate.draw gs_1 User 1 
let gs_12 = Gamestate.draw gs_1 Player 1
let gs_13 = Gamestate.draw gs_1 User 2 
let gs_14 = Gamestate.draw gs_13 Player 1 

(* User and Player each call uno_defensive *)
let gs_15 = Gamestate.uno_defensive gs_1 User 
let gs_16 = Gamestate.uno_defensive gs_15 Player

(* User and Player each call uno_offensive *)
let gs_17 = Gamestate.uno_offensive gs_1 User Player 
let gs_18 = Gamestate.uno_offensive gs_1 Player User 

(* User and Player each win *)
let gs_19 = Gamestate.play gs_15 User "Red 0"
let gs_20 = Gamestate.play gs_1 Player "Red 1"



(* User and Player start out with 2 cards in hand each *)
let gs_2 = from_json_unshuffled json_test 2 

let gs_21 = Gamestate.play gs_2 User "Red 0"
let gs_22 = Gamestate.play gs_21 Player "Red 2"

(* draw pile is empty. discard pile is shuffled*)
let gs_23 = Gamestate.draw gs_22 User 2 
let gs_24 = Gamestate.draw gs_23 Player 1



(* Helper functions to test exceptions*)
let exn_test_1 gs g card_name =
  (try gs_1 = (Gamestate.play gs g card_name) 
   with (Gamestate.CardNotInHand card)-> (card = card_name))

let exn_test_2 gs g card_name = 
  try gs_1 = (Gamestate.play gs g card_name)
  with (Gamestate.MisMatch card)-> (card = card_name)

let exn_test_3 gs g = 
  try gs_1 = (Gamestate.uno_defensive gs g) 
  with (Gamestate.Nouno g)-> true

let exn_test_4 gs g1 g2 = 
  try gs_1 = (Gamestate.uno_offensive gs g1 g2) 
  with (Gamestate.Nouno g) -> (g = g1) 


let gamestate_tests =
  [
    (* testing from_json *)
    (*.......................................................................*)
    (* Is the correct no. of cards dealt to the gamers? *)
    (* Assuming that hand_size works correctly *)
    "from_json_1" >:: (fun _ -> assert_equal 1 (hand_size gs_1 User));
    "from_json_2" >:: (fun _ -> assert_equal 1 (hand_size gs_1 Player));

    (* Is the correct order maintained when dealing cards? *)
    (* Assuming hand works correctly *)
    "from_json_3" >:: (fun _ -> assert_equal ["Red 0"] (hand gs_1 User));
    "from_json_4" >:: (fun _ -> assert_equal ["Red 1"] (hand gs_1 Player));  
    "from_json_5" >:: (fun _ -> assert_equal 
                          ["Red 1"; "Red 0"] (hand gs_2 User));
    "from_json_6" >:: (fun _ -> assert_equal 
                          ["Red 3"; "Red 2"] (hand gs_2 Player));

    (* Is the correct card in the discard pile? *)
    "from_json_7" >:: (fun _ -> assert_equal "Red 2" (last_card_played gs_1));
    "from_json_8" >:: (fun _ -> assert_equal "Red 4" (last_card_played gs_2));



    (* testing number_search *)
    (*.......................................................................*)

    "ns_test1" >:: (fun _-> assert_equal 1 (number_search gs_1 Player "Red 1"));
    "ns_test2" >:: (fun _-> assert_equal 3 (number_search gs_2 Player "Red 3"));


    (* testing color_search *)
    (*.......................................................................*)

    "cs_test1">::(fun _ -> assert_equal "red" (color_search gs_1 User "Red 0"));
    "cs_test2">::(fun _ -> assert_equal "red" 
                     (color_search gs_2 Player "Red 2"));


    (* testing draw *)
    (*.......................................................................*)

    "draw_test_1" >:: (fun _ -> assert_equal 
                          ["Red 3";"Red 0"] (hand gs_11 User)); 
    "draw_test_2" >:: (fun _ -> assert_equal 
                          ["Red 3";"Red 1"] (hand gs_12 Player)); 
    "draw_test_3" >:: (fun _ -> assert_equal
                          ["Red 4"; "Red 3"; "Red 0"] (hand gs_13 User));
    "draw_test_4" >:: (fun _ -> assert_equal 
                          ["Red 5"; "Red 1"] (hand gs_14 Player));
    "draw_test_5" >:: (fun _ -> assert_equal 
                          ["Red 6"; "Red 5"; "Red 1"] (hand gs_23 User));

    (* testing that draw shuffles discard pile to draw pile when necessary*)

    "draw_test_6" >:: (fun _ -> assert_equal "Red 2" (last_card_played gs_24));

    "draw_test_7" >:: (fun _ -> assert_equal
                          ["Red 0"; "Red 3"] (hand gs_24 Player));


    (* testing play *)
    (*.......................................................................*)

    "play_test_1" >:: (fun _ -> assert_equal ["Red 1"] (hand gs_21 User));
    "play_test_2" >:: (fun _ -> assert_equal "Red 0" (last_card_played gs_21));

    "play_test_3" >:: (fun _ -> assert_equal ["Red 3"] (hand gs_22 Player));
    "play_test_4" >:: (fun _ -> assert_equal "Red 2" (last_card_played gs_22));

    (*Find a way to test the right exceptions are thrown *)
    "play_test_5" >:: (fun _ -> assert_equal true 
                          (exn_test_1  gs_2 User "Green 0"));
    "play_test_5" >:: (fun _ -> assert_equal true
                          (exn_test_1  gs_2 Player "Red 00"));  

    (* testing uno_defensive*) 
    (*.......................................................................*)

    "uno_d_test_1" >:: (fun _ -> assert_equal true (uno_state gs_15 User));
    "uno_d_test_2" >:: (fun _ -> assert_equal false (uno_state gs_15 Player));
    "uno_d_test_3" >:: (fun _ -> assert_equal true (uno_state gs_16 Player));
    "uno_d_test_4" >:: (fun _ -> assert_equal true (exn_test_3 gs_2 User)); 


    (* testing uno_offensive *)   
    (*.......................................................................*)

    "uno_o_test_1" >:: (fun _ -> assert_equal 5 (hand_size gs_17 Player)); 
    "uno_o_test_2" >:: (fun _ -> assert_equal 5 (hand_size gs_18 User));
    "uno_o_test_3" >:: (fun _ -> assert_equal true 
                           (exn_test_4 gs_16 User Player));
    "uno_o_test_4" >:: (fun _ -> assert_equal true 
                           (exn_test_4 gs_2 User Player));

    (* testing win_or_not *)
    (*.......................................................................*)

    "win_test_1" >:: (fun _ -> assert_equal true (win_or_not gs_19 User)); 
    "win_test_2" >:: (fun _ -> assert_equal false (win_or_not gs_20 Player))
  ]

(*.........................testing command below..............................*)

let json_com = Yojson.Basic.from_file "test_deck_command.json"

let gs_c = from_json_unshuffled json_test 1
(* for draw *)
let gs_c1 = Gamestate.draw gs_c User 1
let gs_c2 = Gamestate.draw gs_c Player 1
(* for play *)
let gs_c3 = Gamestate.play gs_c1 User "Red 0"
let gs_c4 = Gamestate.play gs_c2 Player "Red 3"
let gs_c5 = Gamestate.draw gs_c1 User 1
(* for uno *)
let gs_c6 = Gamestate.draw gs_c2 Player 1
let gs_c7 = Gamestate.uno_defensive gs_c3 User
let gs_c8 = Gamestate.uno_defensive gs_c4 Player
(* for uno2 *)
(* let gs_c9 = Gamestate.uno_offensive gs_c1 User Player
   let gs_c10 = Gamestate.uno_offensive gs_c2 Player User *)

let command_tests =
  [
    (* test out parse *)
    "pars_draw" >:: (fun _ -> assert_equal 
                        Draw (Command.parse " drAw "));
    "pars_play" >:: (fun _ -> assert_equal 
                        (Play "Red 2") (Command.parse "PlaY Red 2"));
    "pars_uno" >:: (fun _ -> assert_equal 
                       (Uno "Blue 0") (Command.parse "uNo bLue 0"));
    "pars_uno2" >:: (fun _ -> assert_equal 
                        Uno2 (Command.parse "uno2 "));
    "pars_rules" >:: (fun _ -> assert_equal
                         Rules (Command.parse "    ruLes"));
    "pars_commands" >:: (fun _ -> assert_equal 
                            Commands (Command.parse "COmmands "));
    "pars_empty" >:: (fun _ -> assert_raises 
                         Empty (fun () -> Command.parse "   "));
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
    "play_user_legal" >:: (fun _ -> assert_equal
                              (Legal gs_c3) (Command.play gs_c1 User "Red 0"));
    "play_player_legal" >:: (fun _ -> assert_equal
                                (Legal gs_c4) (Command.play gs_c2 Player "Red 3"));
    "play_not_in_hand" >:: (fun _ -> assert_equal
                               (Illegal "You tried to play a card not in your hand: Red 4")
                               (Command.play gs_c1 User "Red 4"));
    (* "play_mismatch" >:: (fun _ -> assert_equal
                            (Illegal "Your card does not match the card last played: Blue 0")
                            (Command.play gs_c5 User "Blue 0")); *)
    (* ---------------------------------------------------------------------- *)
    (* test out uno *)
    "uno_not_in_hand" >:: (fun _ -> assert_equal
                              (Illegal "You tried to play a card not in your hand: Red 4")
                              (Command.uno gs_c1 User "Red 4"));
    (* "uno_mismatch" >:: (fun _ -> assert_equal
                        (Illegal "Your card does not match the card last played: Blue 0")
                        (fun () -> (Command.uno gs_c5 User "Blue 0")); *)
    "uno_user_illegal" >:: (fun _ -> assert_equal
                               (Illegal "nouno") (Command.uno gs_c5 User "Red 3"));
    (* "uno_user_legal" >:: (fun _ -> assert_equal
                             (Legal gs_c7) (Command.uno gs_c1 User "Red 3")); *)
    "uno_player_illegal" >:: (fun _ -> assert_equal
                                 (Illegal "nouno") (Command.uno gs_c6 Player "Red 1"));
    (* "uno_player_legal" >:: (fun _ -> assert_equal
                             (Legal gs_c8) (Command.uno gs_c2 Player "Red 3")); *)
    (* ---------------------------------------------------------------------- *)
    (* test out uno2 *)
    (* "uno2_user_illegal" >:: (fun _ -> assert_equal
                                (Illegal "nouno") (Command.uno2 gs_c2 User Player)); *)
    (* "uno2_user_legal" >:: (fun _ -> assert_equal
                              (Legal gs_c9) (Command.uno2 gs_c1 User Player)); *)

  ]


open Player

(* Making gamestates to test player: *)

(* gs_21 is where User has 1 card and Player has 2, Red 0 is last card played (from above) *)

(* gs_102 is where both gamers have 2 cards and Red 0 is the last card played *)
let gs_101 = Gamestate.draw gs_2 User 1 
let gs_102 = Gamestate.play gs_101 User "Red 0"

(* gs_113 is where User has 2 cards, Player has 3, and Red 0 is the last card played *)
let gs_111 = Gamestate.draw gs_2 User 1 
let gs_112 = Gamestate.draw gs_111 Player 1 
let gs_113 = Gamestate.play gs_112 User "Red 0"

(* In json_test_2, some of the cards are blue instead of red *)
let json_test_2 = Yojson.Basic.from_file "test_deck_2.json"

(* gs_203 is where User has 2 red cards, Player has 3 blue, Red 0 is the last card played *)
let gs_200 = from_json_unshuffled json_test_2 2
let gs_201 = Gamestate.draw gs_200 User 1 
let gs_202 = Gamestate.draw gs_201 Player 1 
let gs_203 = Gamestate.play gs_202 User "Red 0"

(* gs_204 is where User has 2 red cards, Player has 3 blue+1 red, Red 0 is the last card played *)
let gs_204 = Gamestate.draw gs_203 Player 1 

(* gs_205 is where User has 2 red cards, Player has 2 blue, Red 0 is the last card played *)
let gs_205 = Gamestate.play gs_201 User "Red 0"

(* gs_304 is where User has 2 red cards, Player has 2 blue, Blue 0 is the last card played *)
let json_test_3 = Yojson.Basic.from_file "test_deck_3.json"
let gs_300 = from_json_unshuffled json_test_3 2
let gs_301 = Gamestate.draw gs_300 User 1 
let gs_302 = Gamestate.draw gs_301 User 1 
let gs_303 = Gamestate.play gs_302 User "Red 0"
let gs_304 = Gamestate.play gs_303 User "Blue 0"


(** [make_player_turn_test name t expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [player_turn_test t]. *)
let make_player_turn_test
    (name : string) 
    (t)
    (expected_output) : test =
  name >:: (fun _ -> assert_equal (Player.player_turn t) expected_output )

let player_tests =
  [
    (* testing player_turn, which in turn tests the rest of the helper functions in player *)
    make_player_turn_test "uno2" gs_21 (Uno2);
    make_player_turn_test "uno with one playable" gs_102 (Uno "Red 2");
    (*     make_player_turn_test "uno with multiple playable" gs_304 (Uno "Blue 3");
           make_player_turn_test "play with multiple playable" gs_113 (Play "Red 2");
           make_player_turn_test "play with one playable" gs_204 (Play "Red 6");
           make_player_turn_test "draw" gs_203 (Draw);
           make_player_turn_test "draw despite uno" gs_205 (Draw); *)
  ]

let suite =
  "test suite for uno_game"  >::: List.flatten [
    gamestate_tests;
    command_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite