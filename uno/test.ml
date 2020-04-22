open OUnit2
open Gamestate
open Command

let json_uno = Yojson.Basic.from_file "init.json"
let gs_uno = from_json json_uno

let gamestate_tests =
  [

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