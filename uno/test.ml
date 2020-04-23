open OUnit2
open Gamestate
open Command

let json_uno = Yojson.Basic.from_file "init.json"
let gs_7 = from_json_unshuffled json_uno 7 

let json_test = Yojson.Basic.from_file "test_deck.json"

(* User and Player start out with 1 card in hand each *)
let gs_1 = from_json_unshuffled json_test 1
let gs_11 = Gamestate.draw gs_1 User 1 
let gs_12 = Gamestate.draw gs_1 Player 1
let gs_13 = Gamestate.draw gs_1 User 2 

(* User and Player start out with 2 cards in hand each *)
let gs_2 = from_json_unshuffled json_test 2 

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
    "cs_test2">::(fun _ -> assert_equal "red" (color_search gs_2 Player "Red 2"));


    (* testing draw *)
    (*.......................................................................*)

    "draw_test_1" >:: (fun _ -> assert_equal 
                          ["Red 3";"Red 0"] (hand gs_11 User)); 
    "draw_test_2" >:: (fun _ -> assert_equal 
                          ["Red 3";"Red 1"] (hand gs_12 Player)); 
    "draw_test_4" >:: (fun _ -> assert_equal
                          ["Red 4"; "Red 3"; "Red 0"] (hand gs_13 User));


    (* testing player *)
    (*.......................................................................*)


  ]

let command_tests =
  [
    (* test out parse *)
    "pars_draw" >:: (fun _ -> assert_equal Draw (parse " drAw "));
    "pars_play" >:: (fun _ -> assert_equal (Play "red 2") (parse "PlaY Red 2"));
    "pars_uno" >:: (fun _ -> assert_equal (Uno "blue 0") (parse "uNo bLue 0"));
    "pars_uno2" >:: (fun _ -> assert_equal Uno2 (parse "uno2 "));
    "pars_rules" >:: (fun _ -> assert_equal Rules (parse "    ruLes"));
    "pars_commands" >:: (fun _ -> assert_equal Commands (parse "COmmands "));
    "pars_empty" >:: (fun _ -> assert_raises Empty (fun () -> parse "   "));
    "pars_malf" >:: (fun _ -> assert_raises Malformed (fun () -> parse "blaH"));

    (* test out draw *)
    (* test out play *)
    (* test out uno *)
    (* test out uno2 *)
    (* test out rules *)
    (* test out commands *)
  ]

let player_tests =
  [

  ]

let suite =
  "test suite for uno_game"  >::: List.flatten [
    gamestate_tests;
    command_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite