open Gamestate 
open Command

(* This AI, player2, is not your normal player AI! 

   Similar to player, its priorities are, in order: 
   1. Calling Uno2 on the user
   2. Calling Uno for itself, then playing a playable card
   3. Playing a playable card
   4. Drawing if none of the above options are possible

   However, it's a lot more clever when playing cards. 
   1. If the last played card is +2 or +4, it will also play +2 or +4 if on hand
   2. If the user's hand size is 3 or less, it will choose to play action cards;
      if it is greater than that, it will save its action cards
   3. Wildcards are played only if there is no other option, and if played, 
      it will choose the color it has most of in hand

   In cases of multiple playable cards, it prioritizes playing the first card 
   of the same color, then playing the first card of the same number.

   This AI calls Uno and Uno2 when appropriate without fail. *)



(** [number_search t1 hand number gamer] is the name of the first card in [hand] with
    number [number], or an empty string if no such card exists in [hand] *)
let rec number_search t1 hand number gamer =
  match hand with
  | [] -> ""
  | h::t -> if Gamestate.number_search t1 gamer h = number then h 
    else number_search t1 t number gamer

(** [color_search t1 hand color gamer] is the name of the first card in [hand] with
    color [color], or an empty string if no such card exists in [hand] *)
let rec color_search t1 hand color gamer =
  match hand with
  | [] -> ""
  | h::t -> if Gamestate.color_search t1 gamer h = "black"
            || Gamestate.color_search t1 gamer h = color then h 
    else color_search t1 t color gamer

(** [color_search_bounds t1 hand color lwr upr gamer] is the name of the first card in 
    [hand] with color [color] and number between lwr and upr (both inclusive), 
    or an empty string if no such card exists in [hand] *)
let rec color_search_bounds t1 hand color lwr upr gamer =
  match hand with
  | [] -> ""
  | h::t -> if (Gamestate.color_search t1 gamer h = color 
                || Gamestate.color_search t1 gamer h = "black")
            && Gamestate.number_search t1 gamer h >= lwr 
            && Gamestate.number_search t1 gamer h <= upr then h
    else color_search_bounds t1 t color lwr upr gamer



(** [color_count t1 hand color counter gamer] is the number of cards in [hand] 
    with color [color]. *)
let rec color_count t1 hand color counter gamer =
  match hand with
  | [] -> counter
  | h::t -> if Gamestate.color_search t1 gamer h = color 
    then color_count t1 t color (counter+1) gamer
    else color_count t1 t color counter gamer

(** [max_color t1 col_lst color num gamer] is the color of card that [col_lst] 
    contains most of. *)
let rec max_color col_lst color (num:int) gamer =
  match col_lst with 
  | [] -> color
  | (x,y)::t -> if y > num then max_color t x y gamer
    else max_color t color num gamer

(** [wildcard_color t hand gamer] is the color that player will choose when playing a 
    wildcard (i.e., the color that [hand] contains most of), choosing in order of
    Red, Blue, Green, Yellow in the case of ties. *)
let wildcard_color t hand gamer =
  let num_red = color_count t hand "red" 0 gamer in
  let num_blue = color_count t hand "blue" 0 gamer in
  let num_green = color_count t hand "green" 0 gamer in
  let num_yellow = color_count t hand "yellow" 0 gamer in
  let color_list = [("Red", num_red); ("Blue", num_blue); ("Green", num_green); 
                    ("Yellow", num_yellow)] in
  max_color color_list "Red" (0) gamer


(** [find_playable_card t hand gamer] is the first playable card in [hand], 
    prioritizing color then number. Returns an empty string if no such card exists *)
let find_playable_card t hand gamer =
  let same_color_card = color_search t hand (Gamestate.color_state t) gamer in
  let same_num_card = number_search t hand (Gamestate.last_card_played_number t) gamer in
  match (same_color_card) with
  | "" -> 
    (match (same_num_card) with
     | "" -> ""
     | _ -> same_num_card)
  | _ -> same_color_card

(** [find_playable_action_card t hand gamer] is the first playable ACTION card in [hand], 
    prioritizing color then number. Returns an empty string if no such card exists *)
let find_playable_action_card t hand gamer =
  let same_color_card = color_search_bounds t hand (Gamestate.color_state t) 10 14 gamer in
  let same_num_card = number_search t hand (Gamestate.last_card_played_number t) gamer in
  match (same_color_card) with
  | "" -> if Gamestate.last_card_played_number t < 10 then "" else
      (match (same_num_card) with
       | "" -> ""
       | _ -> same_num_card)
  | _ -> same_color_card

(** [find_playable_norm_card t hand gamer] is the first playable NON-ACTION card in [hand], 
    prioritizing color then number. Returns an empty string if no such card exists *)
let find_playable_norm_card t hand gamer =
  let same_color_card = color_search_bounds t hand (Gamestate.color_state t) 0 9 gamer in
  let same_num_card = number_search t hand (Gamestate.last_card_played_number t) gamer in
  match (same_color_card) with
  | "" -> if Gamestate.last_card_played_number t > 9 then "" else
      (match (same_num_card) with
       | "" -> ""
       | _ -> same_num_card)
  | _ -> same_color_card



(** [find_op_card_pt2 t hand gamer] is an action card if the user's hand has 3 or less
    cards, a non action card otherwise, or any card if those are not possible *)
let find_op_card_pt2 t hand gamer =
  let playable_action_card = find_playable_action_card t hand gamer in
  let playable_norm_card = find_playable_norm_card t hand gamer in
  if Gamestate.hand_size t User <= 3 then 
    (match playable_action_card with
     | "" -> find_playable_card t hand gamer
     | _ -> playable_action_card)
  else (match playable_norm_card with
      | "" -> find_playable_card t hand gamer
      | _ -> playable_norm_card)

(** [find_optimal_card t hand gamer] is a +2 or +4 card if last played card is 
    a +2 or +4, funneling to handle other situations if not *)
let find_optimal_card t hand gamer =
  match Gamestate.last_card_played_number t with
  | 12 -> let plus2_inhand = number_search t hand 12 gamer in
    (match plus2_inhand with
     | "" -> ""
     | _ -> plus2_inhand)
  | 14 -> if Gamestate.current_tally_num t != 0 
    then let plus4_inhand = number_search t hand 14 gamer in
      (match plus4_inhand with
       | "" -> ""
       | _ -> plus4_inhand) 
    else find_op_card_pt2 t hand gamer
  | _ -> find_op_card_pt2 t hand gamer

let player_turn t gamer =
  let hand = Gamestate.hand t gamer in
  if Gamestate.hand_size t User = 1 && (Gamestate.uno_state t User = false) then Uno2 User else
    let next_card = find_optimal_card t hand gamer in
    if Gamestate.hand_size t gamer = 2 then match next_card with
      | "" -> Draw
      | _ -> Uno next_card
    else
      match next_card with
      | "" -> Draw
      | _ -> Play next_card

let str_to_command str =
  match str with
  | "Blue" -> Blue
  | "Red" -> Red
  | "Yellow" -> Yellow
  | "Green" -> Green
  | _ -> Red

let choose_color t gamer =
  str_to_command (wildcard_color t (Gamestate.hand t gamer) gamer)