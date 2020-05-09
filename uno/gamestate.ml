open Yojson.Basic.Util
open Stdlib

(*Types*)
type card_name = string

type card = {name: card_name; number: int; color: string}

type gamer = User | Player1 | Player2 | Player3

type deck = card list 

type color = string 

type hand = {gamer: gamer; deck:deck; uno_state: bool}

type hands = hand list 

type decks = {d1: deck; d2: deck}

type hands_deck = {h1: hands; h2: deck}

type tally = {num:int; gamer: gamer}

type t = {draw_pile:deck ;discard_pile:deck ; hands: hands;
          color_state:color; tally:tally}

(*exceptions*)
exception CardNotInHand of card_name 
exception MisMatch of card_name 
exception Nouno of gamer
exception TallyIllegal 
exception InvalidGamer 

(*----------------------------------------------------------------------------*)
(* Functions for init_json and init_json_unshuffled *)

(** [card_of_json] is the card object that [j] represents *)
let card_of_json j = {
  name = j |> member "name" |> to_string; 
  number = j |> member "number" |> to_int; 
  color = j |> member "color" |> to_string
}

(** [shuffle t] returns a new game state with the cards in the Discard pile
    shuffled and reinserted into the Draw pile. This is called by need_shuffle*)
let shuffle (d:deck) : deck  =
  let random = List.map (fun c -> (Random.bits (), c)) d in
  let sorted = List.sort compare random in
  List.map snd sorted

(** [deal initial_decks n] deals out the first [n] cards from [initial_decks].d2 
    to [initial_decks].d2. 
    Precondition: initial_decks is a decks object *)
let rec deal initial_decks (n:int)  = 
  if n = 0 
  then initial_decks 
  else let delt_cards = initial_decks.d1 in 
    let rem_cards = initial_decks.d2 in
    match rem_cards with 
    |[] -> failwith "not enough cards"
    |h::t -> deal {d1 = h::delt_cards; d2 = t} (n-1)


(**[from_json_init json] is the unshuffled list of card objects extracted from 
   the deck of cards in [json] *)
let from_json_init json = json 
                          |> member "cards" |> to_list |> List.map card_of_json

(**[init_colorstate] initializes the color_state as the color of the card on 
   top of the [discard_pile], and if the card is a Wild card, it sets it to 
   "red"*)
let init_colorstate discard_pile= 
  let card = (List.hd discard_pile) in 
  if (card.color = "black") then "red" else card.color

(**[init_hands] initializes the gamers' hands depending on how many AI's are in 
   the game. *)
let init_hands (deck:deck) num_g num : hands_deck = 

  let deal_to_user = deal {d1 = []; d2 = deck} num in 
  let user_hand = {gamer = User; deck=deal_to_user.d1; uno_state = false} in
  let deal_to_player1 = deal {d1 = []; d2 = deal_to_user.d2} num in 
  let player1_hand = {gamer = Player1; 
                      deck = deal_to_player1.d1; uno_state = false} in 
  if (num_g = 2) then 
    let deal_to_player2 = deal {d1 = []; d2 = deal_to_player1.d2} num in 
    let player2_hand = {gamer = Player2; 
                        deck = deal_to_player2.d1; uno_state = false} in
    {h1  = [user_hand; player1_hand; player2_hand]; h2 = deal_to_player2.d2}

  else 
  if (num_g = 3) then
    let deal_to_player2 = deal {d1 = []; d2 = deal_to_player1.d2} num in 
    let player2_hand = {gamer = Player2;
                        deck = deal_to_player2.d1; uno_state = false} in
    let deal_to_player3 = deal {d1 = []; d2 = deal_to_player2.d2} num in 
    let player3_hand = {gamer = Player3; 
                        deck = deal_to_player3.d1; uno_state = false} in
    {h1 = [user_hand; player1_hand; player2_hand; player3_hand]; 
     h2 = deal_to_player3.d2}

  else 
    {h1 = [user_hand; player1_hand]; 
     h2 = deal_to_player1.d2}


let from_json j num num_g =
  let cards = from_json_init j in 
  let shuffled_cards = shuffle cards in 
  let init_hands = init_hands shuffled_cards num_g num in 
  let hands = init_hands.h1 in 
  let cards_left = init_hands.h2 in 
  let discard_pile = [List.hd cards_left] in 
  let draw_pile = List.tl cards_left in 
  {
    draw_pile = draw_pile; 
    discard_pile = discard_pile; 
    hands = hands;
    color_state = (init_colorstate discard_pile);
    tally = {num = 0; gamer = User}
  }

let from_json_unshuffled j num num_g = 
  let cards = from_json_init j in 
  let init_hands = init_hands cards num_g num in 
  let hands = init_hands.h1 in 
  let cards_left = init_hands.h2 in 
  let discard_pile = [List.hd cards_left] in 
  let draw_pile = List.tl cards_left in 
  {
    draw_pile = draw_pile; 
    discard_pile = discard_pile; 
    hands = hands;
    color_state = init_colorstate discard_pile;
    tally = {num = 0; gamer = User}
  }

(*----------------------------------------------------------------------------*)
(* Information that the other modules need *)

let last_card_played t = (List.hd t.discard_pile).name

let last_card_played_color t = (List.hd t.discard_pile).color

let color_state t = t.color_state

let last_card_played_number t = (List.hd t.discard_pile).number

let current_tally_num t = t.tally.num

let current_tally_gamer t = t.tally.gamer

(**[name_of_card c] is the name of the card [c] *)
let name_of_card (c:card) = c.name 

(**[get_gamer_hand hand_lst gamer] is [gamer]'s hand in [hand_lst] 
   Raises: InvalidGamer if [gamer] has no hand in [hand_lst]*)
let rec get_gamer_hand (hand_lst:hands) (gamer) : hand = 
  match hand_lst with 
  |[] -> raise InvalidGamer 
  |h::t -> if (h.gamer = gamer) then h else get_gamer_hand t gamer

let hand t gamer = 
  let hand_lst = t.hands in 
  let deck = (get_gamer_hand hand_lst gamer).deck in 
  List.map name_of_card deck


let hand_size t gamer = List.length (hand t gamer)

let uno_state t gamer = 
  let hand_lst = t.hands in
  (get_gamer_hand hand_lst gamer).uno_state

(*----------------------------------------------------------------------------*)

(**[card_of_card_name lst card_name] is the firt card object with name 
   [card_name] in [lst] *)
let rec card_of_card_name lst card_name  = 
  match lst with 
  |[] -> raise (CardNotInHand card_name) 
  |h::t -> if (h.name = card_name) then h else card_of_card_name t card_name

let number_search t gamer card_name = 
  let hand_lst = t.hands in 
  let deck = (get_gamer_hand hand_lst gamer).deck in 
  (card_of_card_name deck card_name).number


let color_search t gamer card_name = 
  let hand_lst = t.hands in 
  let deck = (get_gamer_hand hand_lst gamer).deck in 
  (card_of_card_name deck card_name).color

(*----------------------------------------------------------------------------*)


(** [need_shuffle t] returns whether the discard pile needs shuffling. This will
    be called from Draw. *)
let need_shuffle t num = (List.length t.draw_pile < num)

(** [shuffle_discard_and_draw] returns the new [t] object after all the cards 
    in the discard pile except for the last played card is added to the draw 
    pile, which is then shuffled. *)
let shuffle_discard_and_draw t =
  let draw = t.draw_pile in  
  let discard = t.discard_pile in 
  let new_discard = [List.hd discard] in 
  let new_draw = shuffle(draw@(List.tl discard)) in
  {draw_pile = new_draw; discard_pile = new_discard; 
   hands = t.hands; color_state = t.color_state; tally = t.tally}

(**[update_tally t num gamer] is the new gamestate after [t].tally.num is 
    updated to [num] and [t].tally.gamer is updated to [gamer]*)
let update_tally t num gamer = 
  {draw_pile = t.draw_pile; discard_pile = t.discard_pile; 
   hands = t.hands; color_state = t.color_state; 
   tally = {num = num; gamer = gamer}}

(**[update_hand lst gamer hand new_lst] is the new [hands] object after the
   [gamer]'s hand is changed to [hand] *)
let rec update_hand (lst:hands) gamer hand new_lst : hands = 
  match lst with 
  |[] -> new_lst 
  |h::t -> if (h.gamer = gamer) 
    then (List.rev (hand::new_lst))@t 
    else update_hand t gamer hand (h::new_lst)


(**[draw_helper t gamer num] is the new [t] object after [num] no. of cards 
   have been drawn and added to the [gamer]'s hand.  *)
let rec draw_helper t gamer num =
  let num_of_cards = t.tally.num in 
  if (t.tally.gamer = gamer && num_of_cards <> 0) 
  then forced_draw_for_tally t gamer num_of_cards
  else let draw = t.draw_pile in 
    let hand_lst = t.hands in 
    let hand = (get_gamer_hand hand_lst gamer) in 
    let deck = hand.deck in 
    let drawn_by_gamer = deal {d1 = []; d2 = draw} num in
    let new_gamer_hand = {gamer = gamer; deck = (drawn_by_gamer.d1)@(deck);
                          uno_state = hand.uno_state} in 
    let new_hands = update_hand hand_lst gamer new_gamer_hand [] in 
    {draw_pile = drawn_by_gamer.d2;
     discard_pile = t.discard_pile; 
     hands = new_hands;
     color_state = t.color_state;
     tally = t.tally}

(**[forced_draw_for_tally t gamer n] forces [gamer] to draw the number of cards
   in the tally in [t] if the tally is against the [gamer]*) 
and forced_draw_for_tally t gamer n = 
  let new_t = update_tally t 0 gamer in 
  draw_helper new_t gamer n 

(**[check_uno_state t gamer] checks if [gamer]'s uno_state is true, and if so, 
   changes it to false.*)
let check_uno_state t gamer = 
  let hands = t.hands in 
  let hand = get_gamer_hand hands gamer in 
  if (hand.uno_state = true) then 
    let new_hand = {gamer = gamer; deck = hand.deck; uno_state = false} in 
    let new_hands = update_hand hands gamer new_hand [] in 
    {draw_pile = t.draw_pile;
     discard_pile = t.discard_pile; 
     hands = new_hands;
     color_state = t.color_state;
     tally = t.tally}
  else t


let draw t gamer num = 
  let t = check_uno_state t gamer in 
  if (need_shuffle t num) 
  then let new_t = shuffle_discard_and_draw t in 
    draw_helper new_t gamer num 
  else draw_helper t gamer num 

let just_to_test t = (List.map name_of_card t.draw_pile)

(*----------------------------------------------------------------------------*)

(**[deck_without_card lst card init] is the list [lst] without [card] *)
let rec deck_without_card lst card init =
  match lst with 
  |[] -> failwith "card not in list"
  |h::t -> if (h = card) then (t@init) else deck_without_card t card (h::init)

(*make wild not legal after +4 or +2*)
let legal_play_or_not t card = 
  let card2 = card_of_card_name t.discard_pile (last_card_played t) in 
  (card.color = t.color_state || card.number = card2.number || 
   card.color = "black" )

(*[update_color t color] updates [t]'s color_state to [color]
  Assumes: [color] is a valid Uno color. *)
let update_color t color = 
  {draw_pile = t.draw_pile; discard_pile = t.discard_pile; 
   hands = t.hands; color_state = color; tally = t.tally}

(** [play_helper t gamer card] is new gamestate after [gamer] successfully 
    plays [card]: 
    [card] is removed from [gamer]'s deck and put into the discard pile *)
let play_helper_4 t gamer card =
  let hand_lst = t.hands in 
  let hand = (get_gamer_hand hand_lst gamer) in 
  let deck = hand.deck in 
  let new_deck = deck_without_card deck card [] in
  let new_hand = {gamer = gamer; deck = new_deck; uno_state = hand.uno_state} in 
  let new_hands = update_hand hand_lst gamer new_hand [] in 
  {draw_pile = t.draw_pile; 
   discard_pile = card::(t.discard_pile); 
   hands = new_hands;
   color_state = t.color_state; 
   tally = t.tally}


(**[play_helper_3 t gamer n_gamer card] checks if [card] is a +2 and changes 
   color_state and tally according. It also adjusts color_state if a normal 
   card is played that matches number and not color. *)
let play_helper_3 t gamer n_gamer card = 
  if card.number = 12 then (* a +2 is thrown *)
    let updated_tally = update_tally t 2 n_gamer in 
    let updated_color = update_color updated_tally (card.color) in 
    play_helper_4 updated_color gamer card
  else if (card.color <> t.color_state) 
  then (*the card has same number but not same color*)
    play_helper_4 (update_color t card.color) gamer card 
  else (*the card has same color, no change in color_state needed*)
    play_helper_4 t gamer card


(* [play_helper_2 t gamer n_gamer card color_str] if tally = 0, and a card of 
   color "black" has been played. appropriate changes are made to t: color_state
   and tally (if +4 is played) *)
let play_helper_2 t gamer n_gamer card color_str = 
  (* a wild is thrown, color_state is changed *)
  if (card.color = "black" && card.number = 13)  
  then play_helper_4 (update_color t color_str) gamer card
  (* a +4 is thrown, tally is updated, and color_state is changed*)
  else if (card.color = "black" && card.number = 14) 
  then let new_t = update_color t color_str in
    play_helper_4 (update_tally new_t 4 n_gamer) gamer card 
  else play_helper_3 t gamer n_gamer card


(* [legal_play_tally t lcard card gamer color_str] is the new gamestate with 
   adjustments made [card] is played by [gamer] when there is an active tally
   them.  
   Raises: TallyIllegal if the gamer tries to play a card that doesn't match 
   the tally or the earlier played action card *)
let legal_play_tally t lcard card gamer color_str= 
  if (lcard.number = 12 && card.number = 12 ) (*a +2 is countered with a +2*)
  then update_color (update_tally t (t.tally.num + 2) gamer)(card.color)
  else if (lcard.number = 12 && card.number = 14) (*+2 is countered with a +4*)
  then update_color (update_tally t (t.tally.num + 4) gamer) color_str
  else if (lcard.number = 14 && card.number = 14) (*+4 is countered with a +4*)
  then update_color (update_tally t (t.tally.num + 4) gamer) color_str
  else raise (TallyIllegal)


(** [play_helper_1 t gamer n_gamer card color_str] checks if there is a non-zero
    tally against [gamer], and if they can play [card] against the last action 
    card played. If its a legal move, it returns the gamestate t after updating 
    tally, and color_state (if a wild+4 is played), or it throws TallyIllegal.*)
let play_helper_1 t gamer n_gamer card color_str = 
  if (t.tally.num <> 0 && t.tally.gamer = gamer)  
  then let lcard = List.hd t.discard_pile in 
    let new_t = (legal_play_tally t lcard card n_gamer color_str) in 
    (play_helper_4 new_t gamer card)
  else (play_helper_2 t gamer n_gamer card color_str)


let play t gamer n_gamer card_name color_str = 
  let hand_lst = t.hands in 
  let hand = (get_gamer_hand hand_lst gamer) in 
  let deck = hand.deck in 
  let card = card_of_card_name deck card_name in 
  if (legal_play_or_not t card) then 
    play_helper_1 t gamer n_gamer card color_str
  else raise (MisMatch card_name)

(*---------------------------------------------------------------------------*)
(**[uno_state_change t gamer] is the gamestate where the uno_state of the 
   [gamer] is changed to true.  *)
let uno_state_change t gamer = 
  let hand_lst = t.hands in 
  let hand = (get_gamer_hand hand_lst gamer) in 
  let new_hand = {gamer = gamer; deck = hand.deck; uno_state = true} in 
  let new_hands = update_hand hand_lst gamer new_hand [] in 
  {draw_pile = t.draw_pile; 
   discard_pile = t.discard_pile; 
   hands = new_hands;
   color_state = t.color_state;
   tally = t.tally}


let uno_defensive t gamer =
  if ((hand_size t gamer) = 1)
  then uno_state_change t gamer 
  else raise (Nouno gamer)

(*----------------------------------------------------------------------------*)

let uno_offensive t gamer1 gamer2 = 
  let hand_lst = t.hands in 
  let hand = (get_gamer_hand hand_lst gamer2) in 
  if (hand_size t gamer2 = 1 && hand.uno_state = false)
  then (draw t gamer2 4)
  else raise (Nouno gamer2)

(*----------------------------------------------------------------------------*)

let win_or_not t gamer = 
  let hand_lst = t.hands in 
  let hand = (get_gamer_hand hand_lst gamer) in 
  (hand_size t gamer = 0 && hand.uno_state = true)

(*----------------------------------------------------------------------------*)