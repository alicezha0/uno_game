open Gamestate 
open Command

(* This AI, player, priorities in order: 
   1. Calling Uno2 on the user
   2. Calling Uno for itself, then playing a playable card
   3. Playing a playable card
   4. Drawing if none of the above options are possible

   When playing a card, it prioritizes playing the first card of the same color, 
   then playing the first card of the same number. When playing a wildcard, 
   it will choose a random color.

   This AI calls Uno and Uno2 when appropriate without fail. *)


(** [number_search hand number] is the name of the first card in [hand] with
    number [number], or an empty string if no such card exists in [hand] *)
let rec number_search t1 hand number gamer =
  match hand with
  | [] -> ""
  | h::t -> if Gamestate.number_search t1 gamer h = number then h 
    else number_search t1 t number gamer

(** [color_search hand color] is the name of the first card in [hand] with
    color [color], or an empty string if no such card exists in [hand] *)
let rec color_search t1 hand color gamer =
  match hand with
  | [] -> ""
  | h::t -> if Gamestate.color_search t1 gamer h = "black"
            || Gamestate.color_search t1 gamer h = color 
    then h 
    else color_search t1 t color gamer

(** [find_playable_card hand card_name] is the first playable card in [hand], 
    prioritizing color then number. Returns an empty string if no such card exists *)
let find_playable_card t hand card_name gamer =
  let same_color_card = color_search t hand (Gamestate.color_state t) gamer in
  let same_num_card = number_search t hand (Gamestate.last_card_played_number t) gamer in
  match (same_num_card) with
  | "" -> 
    (if (Gamestate.last_card_played_number t != 12 
         && Gamestate.last_card_played_number t != 14) then 
       (match (same_color_card) with
        | "" -> ""
        | _ -> same_color_card) 
     else "")
  | _ -> same_num_card


let player_turn t gamer =
  let hand = Gamestate.hand t gamer in
  let last_card = Gamestate.last_card_played t in 
  if Gamestate.hand_size t User = 1 && (Gamestate.uno_state t User = false) 
  then Uno2 User else
    let playable_card = find_playable_card t hand last_card gamer in
    if Gamestate.hand_size t gamer = 2 then match playable_card with
      | "" -> Draw
      | _ -> Uno playable_card
    else
      match playable_card with
      | "" -> Draw
      | _ -> Play playable_card

let choose_color t =
  match Random.int 4 with
  | 0 -> Red
  | 1 -> Blue
  | 2 -> Green
  | 3 -> Yellow
  | _ -> Red