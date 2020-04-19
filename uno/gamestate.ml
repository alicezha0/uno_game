open Yojson.Basic.Util

type t 
type card_name = string
type gamer = User | Player

exception UnknownCard of card_name
exception CardNotInDeck of card_name 
exception MisMatch of card_name 
exception Nouno of gamer


let from_json j =
  failwith "Unimplemented"

let last_card_played t =
  failwith "Unimplemented"

let hand t gamer =
  failwith "Unimplemented"

let hand_size t gamer =
  failwith "Unimplemented"

(** [shuffle t] returns a new game state with the cards in the Discard pile
    shuffled and reinserted into the Draw pile. This is called by need_shuffle *)
let shuffle t =
  failwith "Unimplemented"

(** [need_shuffle t] returns whether the discard pile needs shuffling. This will
    be called from Draw. *)
let need_shuffle t =
  failwith "Unimplemented"

let draw t gamer num =
  failwith "Unimplemented"

let draw t gamer card_name =
  failwith "Unimplemented"

let uno_defensive t gamer =
  failwith "Unimplemented"

let uno_offensive t gamer =
  failwith "Unimplemented"

let win_or_not t gamer = 
  failwith "Unimplemented"