open Gamestate
open Yojson.Basic.Util
open Stdlib

type card_phrase = string

type command = 
  | Draw
  | Play of card_phrase
  | Uno of card_phrase
  | Uno2 of Gamestate.gamer
  | Rules
  | Commands
  | Quit

type result =
  | Illegal of string
  | Legal of Gamestate.t

type color =
  | Red
  | Yellow
  | Green
  | Blue
  | Any

exception Empty
exception Malformed

(** [no_empty str_list acc] is the string list [acc] without any empty
    spaces as elements from [str_list]. *)
let rec no_empty str_list acc =
  match str_list with 
  | [] -> acc 
  | h::t -> if h = "" then no_empty t acc else no_empty t (h::acc)

(** [parse_helper str] is the type command that has a card phrase attached to 
    it.
    Raises: Malformed if the input does not match Play, Uno, or Uno2. *)
let parse_helper str =
  let split_str = String.split_on_char ' ' str in 
  let remove_empty = List.rev (no_empty split_str []) in 
  let list_head = List.hd remove_empty in 
  let caps_first = List.map String.capitalize_ascii (List.tl remove_empty) in
  let phr = String.concat " " caps_first in
  if list_head = "play" then Play phr
  else if list_head = "uno" then Uno phr
  else if list_head = "uno2" then 
    if phr = "Player1" then Uno2 Player1
    else if phr = "Player2" then Uno2 Player2
    else if phr = "Player3" then Uno2 Player3
    else if phr = "User" then Uno2 User
    else raise Malformed
  else raise Malformed

let parse str =
  let trim_lc = str |> String.trim |> String.lowercase_ascii in
  if trim_lc = "" then raise Empty 
  else if trim_lc = "rules" then Rules
  else if trim_lc = "commands" then Commands
  else if trim_lc = "draw" then Draw
  else if trim_lc = "quit" then Quit
  else if trim_lc = "play" then raise Malformed
  else if trim_lc = "uno" then raise Malformed
  else parse_helper trim_lc

let parse_color str =
  let trim_lc = str |> String.trim |> String.lowercase_ascii in 
  if trim_lc = "" then raise Empty
  else if trim_lc = "red" then Red
  else if trim_lc = "yellow" then Yellow 
  else if trim_lc = "green" then Green
  else if trim_lc = "blue" then Blue
  else raise Malformed

let draw gs gamer num =
  Legal (Gamestate.draw gs gamer num)

(** [color_to_string clr] is the string that corresponds to the type color. *)
let color_to_string clr =
  if clr = Red then "red"
  else if clr = Yellow then "yellow"
  else if clr = Green then "green"
  else if clr = Blue then "blue"
  else ""

(** [tally_illegal gs] is the Illegal state with a helpful error message after
    an illegal move has been played on top of a +2 or +4 card. *)
let tally_illegal gs =
  let num_last_card = Gamestate.last_card_played_number gs in 
  if num_last_card = 12 then
    Illegal ("\nThe last card played was a +2, which means you must draw \
              cards if you do not have a +2 or +4 card to play.")
  else
    Illegal ("\nThe last card played was a +4, which means you must draw \
              cards if you do not have a +4 card.")

let play gs gamer n_gamer (phr:card_phrase) clr =
  let clr_str = color_to_string clr in
  match Gamestate.play gs gamer n_gamer phr clr_str with 
  | exception Gamestate.CardNotInHand card -> 
    Illegal ("\nYou tried to play a card not in your hand: " ^ card)
  | exception Gamestate.MisMatch card -> 
    Illegal ("\nYour card does not match the card last played: " ^ card)
  | exception Gamestate.TallyIllegal -> tally_illegal gs
  | _ -> Legal (Gamestate.play gs gamer n_gamer phr clr_str)

let uno gs gamer n_gamer (phr:card_phrase) clr =
  let clr_str = color_to_string clr in 
  match Gamestate.play gs gamer n_gamer phr clr_str with
  | exception Gamestate.CardNotInHand card ->
    Illegal ("\nYou tried to play a card not in your hand: " ^ card)
  | exception Gamestate.MisMatch card ->
    Illegal ("\nYour card does not match the card last played: " ^ card)
  | exception Gamestate.TallyIllegal -> tally_illegal gs
  | _ -> begin
      match Gamestate.uno_defensive 
              (Gamestate.play gs gamer n_gamer phr clr_str) gamer with 
      | exception Gamestate.Nouno gamer -> Illegal "nouno"
      | _ -> Legal (Gamestate.uno_defensive 
                      (Gamestate.play gs gamer n_gamer phr clr_str) gamer)
    end

let uno2 gs gamer1 gamer2 =
  match Gamestate.uno_offensive gs gamer1 gamer2 with 
  | exception Gamestate.Nouno gamer -> 
    Illegal ("\nYou did not call a valid offensive uno. The other player does \
              not have Uno. You have been forced to draw 4 cards.")
  | exception Gamestate.InvalidGamer ->
    Illegal ("\nYou did not call a valid offensive uno on a player that is \
              in the game.")
  | _ -> Legal (Gamestate.uno_offensive gs gamer1 gamer2)

let rules =
  let json = Yojson.Basic.from_file "rules.json" in 
  let lines each_line = each_line |> member "a" |> to_string in
  let rules_list = json |> member "rules" |> to_list |> List.map lines in 
  String.concat "\n\n" rules_list

let commands =
  let json = Yojson.Basic.from_file "commands.json" in 
  let lines each_line = each_line |> member "b" |> to_string in
  let commands_list = json |> member "commands" |> to_list |> List.map lines in 
  String.concat "\n\n" commands_list