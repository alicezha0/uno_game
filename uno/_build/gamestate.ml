open Yojson.Basic.Util
open Stdlib

(* ALICE! CAROLINE! WRITING SOME HELPFUL COMMENTS BESIDE NEW TYPES THAT I 
   DEFINED AND TO WALK Y'ALL THROUGH MY IMPLEMENTATIONS. MY HELPER FUNCTIONS 
   WILL ALL BE DOCUMENTED! We can delete these later. *)

type card_name = string

type card = {name: card_name; number: int; color: string}

(* Not sure we need this. maybe our hand function should just extract names from the player_hand and user_hand in t *)
(* type hand = {deck: card_name list ; name:string}*)

(*This is just a list of card objects *)
type deck = card list 


type hand = {deck:deck; uno_state: bool}

(* Had to create this type to implement the deal functions. Basically you want to deal a certain number of cards from
   a deck of cards, but then you want to return both the deck that you have dealt and the deck you dealt from. No way 
   to return two deck objects in one function, also i searched in Ocaml list but didn't find anything.*)
type decks = {d1: deck; d2: deck}

type t = {draw_pile:deck ;discard_pile:deck ;user_hand:hand ;player_hand:hand}

type gamer = User | Player


exception CardNotInHand of card_name 
exception MisMatch of card_name 
exception Nouno of gamer

(** [card_of_json] is the card object that [j] represents *)
let card_of_json j = {
  name = j |> member "name" |> to_string; 
  number = j |> member "number" |> to_int; 
  color = j |> member "color" |> to_string
}

(** [shuffle t] returns a new game state with the cards in the Discard pile
    shuffled and reinserted into the Draw pile. This is called by need_shuffle *)
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
  else 
    match initial_decks with 
    |{d1 = delt_cards ;d2 = rem_cards} -> begin
        match rem_cards with 
        |[] -> failwith "not enough cards"
        |h::t -> deal {d1 = h::delt_cards; d2 = t} (n-1)
      end

(** [from_json_init json] is the unshuffled list of card objects extracted from 
    the deck of cards in [json] *)
let from_json_init json = json 
                          |> member "cards" |> to_list |> List.map card_of_json


let from_json j num =
  let cards = from_json_init j in 
  let shuffled_cards = shuffle cards in 
  let deal_to_user = deal {d1 = []; d2 = shuffled_cards} num in 
  let user_hand = {deck=deal_to_user.d1; uno_state = false} in 
  let deal_to_player = deal {d1 = []; d2 = deal_to_user.d2} num in 
  let player_hand = {deck = deal_to_player.d1; uno_state = false} in 
  let discard_pile = [List.hd deal_to_player.d2] in 
  let draw_pile = List.tl deal_to_player.d2 in 
  {
    draw_pile = draw_pile; 
    discard_pile = discard_pile; 
    user_hand = user_hand; 
    player_hand = player_hand
  }

let from_json_unshuffled j num = 
  let cards = from_json_init j in 
  let deal_to_user = deal {d1 = []; d2 = cards} num in 
  let user_hand = {deck=deal_to_user.d1; uno_state = false} in 
  let deal_to_player = deal {d1 = []; d2 = deal_to_user.d2} num in 
  let player_hand = {deck = deal_to_player.d1; uno_state = false} in 
  let discard_pile = [List.hd deal_to_player.d2] in 
  let draw_pile = List.tl deal_to_player.d2 in 
  {
    draw_pile = draw_pile; 
    discard_pile = discard_pile; 
    user_hand = user_hand; 
    player_hand = player_hand
  }


let last_card_played t = (List.hd t.discard_pile).name

let last_card_played_color t = (List.hd t.discard_pile).color

let last_card_played_number t = (List.hd t.discard_pile).number

(** [name_of_card c] is the name of the card [c] *)
let name_of_card (c:card) = c.name 

let hand t gamer = 
  match gamer with 
  |Player -> List.map name_of_card t.player_hand.deck
  |User -> List.map name_of_card t.user_hand.deck

let hand_size t gamer = List.length (hand t gamer)

let uno_state t gamer = 
  match gamer with 
  |Player -> t.player_hand.uno_state 
  |User -> t.user_hand.uno_state

(*----------------------------------------------------------------------------*)

(**[card_of_card_name lst card_name] is the firt card object with name 
   [card_name] in [lst] *)
let rec card_of_card_name lst card_name  = 
  match lst with 
  |[] -> raise (CardNotInHand card_name) 
  |h::t -> if (h.name = card_name) then h else card_of_card_name t card_name

let number_search t gamer card_name = 
  match gamer with 
  |Player -> let lst = t.player_hand.deck in 
    let card = card_of_card_name lst card_name in 
    card.number
  |User -> let lst = t.user_hand.deck in 
    let card = card_of_card_name lst card_name in 
    card.number


let color_search t gamer card_name = 
  match gamer with 
  |Player -> let lst = t.player_hand.deck in 
    let card = card_of_card_name lst card_name in 
    card.color
  |User -> let lst = t.user_hand.deck in 
    let card = card_of_card_name lst card_name in 
    card.color

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
   user_hand = t.user_hand; player_hand = t.player_hand}

(**[draw_helper t gamer num] is the new [t] object after [num] no. of cards 
   have been drawn and added to the [gamer]'s hand.  *)
let draw_helper t gamer num =
  match gamer with 
  |Player -> let draw = t.draw_pile in 
    let drawn_by_player = deal {d1 = []; d2 = draw} num in
    let new_player_hand = {deck = (drawn_by_player.d1)@(t.player_hand.deck); 
                           uno_state = t.player_hand.uno_state} in
    {draw_pile = drawn_by_player.d2;
     discard_pile = t.discard_pile; 
     user_hand = t.user_hand;
     player_hand = new_player_hand}
  |User -> let draw = t.draw_pile in 
    let drawn_by_user = deal {d1 = []; d2 = draw} num in 
    let new_user_hand = {deck = (drawn_by_user.d1)@(t.user_hand.deck); 
                         uno_state = t.user_hand.uno_state} in
    {draw_pile = drawn_by_user.d2;
     discard_pile = t.discard_pile; 
     user_hand = new_user_hand;
     player_hand = t.player_hand}

let draw t gamer num = 
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

let legal_play_or_not t card = 
  let card2 = card_of_card_name t.discard_pile (last_card_played t) in 
  (card2.color = card.color || card2.number = card.number)


(** [play_helper t gamer card] is new gamestate after [gamer] successfully 
    plays [card] *)
let play_helper t gamer card =
  match gamer with 

  |Player -> let hand = t.player_hand.deck in 
    let new_deck = deck_without_card hand card [] in 
    let new_hand = {deck = new_deck; uno_state = t.player_hand.uno_state} in

    {draw_pile = t.draw_pile; 
     discard_pile = card::(t.discard_pile); 
     user_hand = t.user_hand; 
     player_hand = new_hand}

  |User -> let hand = t.user_hand.deck in 
    let new_deck = deck_without_card hand card [] in 
    let new_hand = {deck = new_deck; uno_state = t.user_hand.uno_state} in

    {draw_pile = t.draw_pile; 
     discard_pile = card::(t.discard_pile); 
     user_hand = new_hand; 
     player_hand = t.player_hand}




let play t gamer card_name = 
  match gamer with 
  (* Remember: our AI does not make mistakes. Might be able to get rid of this 
     part *)
  |Player -> let card = card_of_card_name t.player_hand.deck card_name in 
    if (legal_play_or_not t card) then play_helper t gamer card 
    else raise (MisMatch card_name) 

  |User-> let card = card_of_card_name t.user_hand.deck card_name in 
    if (legal_play_or_not t card) then play_helper t gamer card 
    else raise (MisMatch card_name) 

(*---------------------------------------------------------------------------*)
(**[uno_state_change t gamer] is the gamestate where the uno_state of the 
   [gamer] is changed to true.  *)
let uno_state_change t gamer = 
  match gamer with 
  |Player -> 
    let new_hand = {deck = t.player_hand.deck; uno_state= true} in 
    {draw_pile = t.draw_pile; 
     discard_pile = t.discard_pile; 
     user_hand = t.user_hand; 
     player_hand = new_hand}
  |User -> 
    let new_hand = {deck = t.user_hand.deck; uno_state= true} in 
    {draw_pile = t.draw_pile; 
     discard_pile = t.discard_pile; 
     user_hand = new_hand; 
     player_hand = t.player_hand}



let uno_defensive t gamer =
  match gamer with 
  |Player -> 
    if (hand_size t Player = 1) 
    then uno_state_change t Player 
    else raise (Nouno Player) 
  |User -> 
    if (hand_size t User = 1)
    then uno_state_change t User 
    else raise (Nouno User) 


(*----------------------------------------------------------------------------*)

let uno_offensive t gamer1 gamer2 = 
  match gamer1 with 

  |Player -> if (hand_size t User = 1 && t.user_hand.uno_state = false)
    then (draw t gamer2 4)
    else raise (Nouno gamer2)

  |User -> if (hand_size t Player = 1 && t.player_hand.uno_state = false)
    then (draw t gamer2 4)
    else raise (Nouno gamer1)



(*----------------------------------------------------------------------------*)

let win_or_not t gamer = 
  match gamer with 
  |Player -> (hand_size t Player = 0 && t.player_hand.uno_state = true)
  |User -> (hand_size t User = 0 && t.user_hand.uno_state = true)

(*----------------------------------------------------------------------------*)
