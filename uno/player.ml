open Gamestate 
open Command

type card_name = string 

type command


(* This AI, player, priorities in order: 
   1. Calling Uno2 on the user
   2. Calling Uno for itself, then playing a playable card
   3. Playing a playable card
   4. Drawing if none of the above options are possible

   When playing a card, it prioritizes playing the first card of the same color, 
   then playing the first card of the same number.  


   Right now i only have it calling uno for itself, playing, and drawing *)


(** [number_search hand number] is the name of the first card in [hand] with
    number [number], or an empty string if no such card exists in [hand] *)
let rec number_search hand number =
  match hand with
  | [] -> ""
  | h::t -> if h.number = number then h else number_search t number
(* replace h.number with a way to retrieve its number*)

(** [color_search hand color] is the name of the first card in [hand] with
    color [color], or an empty string if no such card with color [color] exists
    in [hand] *)
let rec color_search hand color =
  match hand with
  | [] -> ""
  | h::t -> if h.color = color then h else color_search t color
(* replace h.color with a way to retrieve its color*)


(** [find_playable_card hand card_name] is the first playable card in [hand], 
    prioritizing color, then number. Returns an empty string if no such exists *)
let find_playable_card hand card_name =
  let same_color_card = color_search hand card_name in
  let same_num_card = number_search hand card_name in
  match (same_color_card) with
  | "" -> 
    (match (same_num_card) with
     | "" -> ""
     | _ -> same_num_card)
  | _ -> same_color_card

(** [player hand card_name] is the command for the next move to be made by the 
    player, depending on their current hand [hand], and the last card 
    played [card]. *)
let player_turn hand card_name = 
  let playable_card = find_playable_card hand card_name in
  if List.length hand = 2 then match playable_card with
    | "" -> Draw
    | _ -> Uno playable_card
  else
    match playable_card with
    | "" -> Draw
    | _ -> Play playable_card