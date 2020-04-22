open Gamestate
open Yojson.Basic.Util
open Stdlib

type card_phrase = string
type t = Gamestate.t
type command = 
  | Draw
  | Play of card_phrase
  | Uno of card_phrase
  | Uno2
  | Rules
  | Commands

type result =
  | Illegal of string
  | Legal of t
  | Legal2 of string

exception Empty
exception Malformed

(** [no_empty str_list acc] is the string list [acc] without any empty
    spaces as elements from [str_list]. *)
let rec no_empty str_list acc =
  match str_list with 
  | [] -> acc 
  | h::t -> if h = "" then no_empty t acc else no_empty t (h::acc)

(** [parse_helper str] is the command that has a card_phrase attached to it.
    Raises: Malformed if the input does not match Play or Uno. *)
let parse_helper str =
  let split_str = String.split_on_char ' ' str in 
  let remove_empty = List.rev (no_empty split_str []) in 
  let list_head = List.hd remove_empty in 
  let card_phr = String.concat " " (List.tl remove_empty) in
  if list_head = "play" then Play card_phr
  else if list_head = "uno" then Uno card_phr
  else raise Malformed

let parse str =
  let trim_lc = str |> String.trim |> String.lowercase_ascii in
  if trim_lc = "" then raise Empty 
  else if trim_lc = "rules" then Rules
  else if trim_lc = "commands" then Commands
  else if trim_lc = "draw" then Draw
  else if trim_lc = "uno2" then Uno2
  else parse_helper trim_lc

let draw t gamer num =
  Legal (Gamestate.draw t gamer num)

let play t gamer phr =
  let try_play = Gamestate.play t gamer phr in 
  match try_play with 
  | exception Gamestate.UnknownCard card -> 
    Illegal ("You played an unknown card: " ^ card)
  | exception Gamestate.CardNotInDeck card -> 
    Illegal ("You tried to play a card not in your deck: " ^ card)
  | exception Gamestate.MisMatch card -> 
    Illegal ("Your card does not match the card last played: " ^ card)
  | _ -> Legal try_play

let uno t gamer phr =
  let try_uno = Gamestate.uno_defensive t gamer phr in 
  match try_uno with 
  | exception Gamestate.UnknownCard card -> 
    Illegal ("You played an unknown card: " ^ card)
  | exception Gamestate.CardNotInDeck card -> 
    Illegal ("You tried to play a card not in your deck: " ^ card)
  | exception Gamestate.MisMatch card -> 
    Illegal ("Your card does not match the card last played: " ^ card)
  | exception Gamestate.Nouno gamer ->
    Illegal ("You did not have two cards left.")
  | _ -> Legal try_uno

let uno2 t gamer1 gamer2 =
  let try_uno2 = Gamestate.uno_offensive t gamer1 gamer2 in 
  match try_uno2 with 
  | exception Gamestate.Nouno gamer -> 
    Illegal ("You did not call a valid offensive uno. The other player does not
    have Uno.")
  | _ -> Legal try_uno2

let rules =
  let json = Yojson.Basic.from_file "rules.json" in 
  let lines each_line = each_line |> member "a" |> to_string in
  let rules_list = json |> member "rules" |> to_list |> List.map lines in 
  Legal2 (String.concat "\n" rules_list)

let commands =
  let json = Yojson.Basic.from_file "commands.json" in 
  let lines each_line = each_line |> member "b" |> to_string in
  let commands_list = json |> member "commands" |> to_list |> List.map lines in 
  Legal2 (String.concat "\n" commands_list) 