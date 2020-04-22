(**
   Parsing of player commands.
*)

(** The type [card_phrase] represents name of the card that will be part of a 
    player command.
    - If the player command is ["Play Red 2"], then the card phrase is 
      ["Red 2"].

    A [card_phrase] is not permitted to be the empty list. *)
type card_phrase = string

(** The type gamestate. *)
type t

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a card_phrase. *)
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

(** Raised when an empty command or card_phrase is parsed. *)
exception Empty

(** Raised when a malformed command or card_phrase is encountered. *)
exception Malformed

(** [parse str] parses a gamer's input into a [command], as follows. The first
    word of [str] becomes the verb. The rest of the words, if any, become the 
    card_phrase.
    Examples: 
    - [parse "play Red 2"] is [Play "Red 2"]
    - [parse "rules"] is [Rules]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [Malformed] if the command is malformed. *)
val parse : string -> command

(** [draw t] is Legal of the new game state in which the gamer has drawn a new
    card. *)
val draw : t -> result

(** [play t phr] is Legal of the new game state or Illegal with an 
    error message. *)
val play : t -> card_phrase -> result

(** [uno t phr] returns Legal of the new game state or Illegal of an error
    message. *)
val uno : t -> card_phrase -> result

(** [uno2 t] returns Legal of the new game state or Illegal of an error
    message. *)
val uno2 : t -> result

(** [rules] is the string of the rules parsed from the rules.json file. *)
val rules : string

(** [commands] is the string of the commands parsed from the commands.json 
    file. *)
val commands : string