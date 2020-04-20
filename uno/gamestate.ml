open Yojson.Basic.Util
open Stdlib

(* ALICE! CAROLINE! WRITING SOME HELPFUL COMMENTS BESIDE NEW TYPES THAT I 
   DEFINED AND TO WALK Y'ALL THROUGH MY IMPLEMENTATIONS. MY HELPER FUNCTIONS 
   WILL ALL BE DOCUMENTED! We can delete these later. *)

type card_name = string
type card = {name: card_name; number: int; color: string}
(* Not sure we need this. maybe our hand function should just extract names from the player_hand and user_hand in t *)
type hand = {deck: card_name list ; name:string}
(* This is just a list of card objects *)
type deck = card list 
(* Had to create this type to implement the deal functions. Basically you want to deal a certain number of cards from
   a deck of cards, but then you want to return both the deck that you have dealt and the deck you dealt from. No way 
   to return two deck objects in one function, also i searched in Ocaml list but didn't find anything.*)
type decks = {d1: deck; d2: deck}
type t = {draw_pile: deck ; discard_pile: deck ; user_hand : deck ; player_hand: deck}

type gamer = User | Player

exception UnknownCard of card_name
exception CardNotInDeck of card_name 
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
  failwith "Unimplemented"


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


let from_json j =
  let cards = from_json_init j in 
  let shuffled_cards = shuffle cards in 
  let deal_to_user = deal {d1 = []; d2 = shuffled_cards} 7 in 
  let user_hand = deal_to_user.d1 in 
  let deal_to_player = deal {d1 = []; d2 = deal_to_user.d2} 7 in 
  let player_hand = deal_to_player.d1 in 
  let discard_pile = [List.hd deal_to_player.d2] in 
  let draw_pile = List.tl deal_to_player.d2 in 
  {
    draw_pile = draw_pile; 
    discard_pile = discard_pile; 
    user_hand = user_hand; 
    player_hand = player_hand
  }


let last_card_played t = (List.hd t.discard_pile).name

(** [name_of_card c] is the name of the card [c] *)
let name_of_card (c:card) = c.name 

let hand t gamer = 
  match gamer with 
  |Player -> List.map name_of_card t.player_hand 
  |User -> List.map name_of_card t.user_hand 

let hand_size t gamer = List.length (hand t gamer)

(** [need_shuffle t] returns whether the discard pile needs shuffling. This will
    be called from Draw. *)
let need_shuffle t = t.draw_pile = [] 

let draw t gamer num =
  match gamer with 
  |Player -> let draw = t.draw_pile in 
    let drawn_by_player = deal {d1 = []; d2 = draw} num in 
    {draw_pile = drawn_by_player.d2; discard_pile = t.discard_pile; 
     user_hand = t.user_hand; player_hand = (drawn_by_player.d1)@t.player_hand}
  |User -> let draw = t.draw_pile in 
    let drawn_by_user = deal {d1 = []; d2 = draw} num in 
    {draw_pile = drawn_by_user.d2; discard_pile = t.discard_pile; 
     user_hand = (drawn_by_user.d1)@t.user_hand; player_hand = t.player_hand}

let play t gamer card_name =
  failwith "Unimplemented"

let uno_defensive t gamer =
  failwith "Unimplemented"

let uno_offensive t gamer =
  failwith "Unimplemented"

let win_or_not t gamer = 
  failwith "Unimplemented"