open GameState
open Command
open Player

(** [play_game f] starts the uno game in file [f]. *)
(* let play_game f = *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Uno. blahblah authors about us? \n");
  (* output rules and commands *)
  (* play_game  *)
  (* print_endline "Please enter the name of the game file you want to load.\n";
     print_string  "> ";
     match read_line () with
     | exception End_of_file -> ()
     | file_name -> play_game file_name *)

  (* Execute the game engine. *)
  let () = main ()