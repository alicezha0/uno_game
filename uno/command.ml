open GameState

type card_phrase = string
type t
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

exception Empty
exception Malformed


let parse str =
  failwith "Unimplemented"

let draw t command =
  failwith "Unimplemented"

let play t command =
  failwith "Unimplemented"

let uno t command =
  failwith "Unimplemented"

let uno2 t command =
  failwith "Unimplemented"

let rules command =
  failwith "Unimplemented"

let commands command =
  failwith "Unimplemented"