(**
   Parsing of player commands.
*)

(** The type [card_phrase] represents name of the card that will be part of a 
    player command.
    - If the player command is ["Play Red 2"], then the card phrase is 
      ["Red 2"].

    A [card_phrase] is not permitted to be the empty list. *)
type card_phrase = string

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a card_phrase. *)
type command = 
  | Draw
  | Play of card_phrase
  | Uno of card_phrase
  | Uno2
  | Rules

(** Raised when an empty command or card_phrase is parsed. *)
exception Empty

(** Raised when a malformed command or card_phrase is encountered. *)
exception Malformed

(** Raised when a card_phrase played is illegal.*) 
exception Mismatch

(** Raised when an uno called is illegal.*) 
exception Nouno

(** [parse str] parses a player's input into a [command], as follows. The first
    word of [str] becomes the verb. The rest of the words, if any, become the 
    card_phrase.
    Examples: 
    - [parse "play Red 2"] is [Play "Red 2"]
    - [parse "rules"] is [Rule]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [Malformed] if the command is malformed (if the verb is none ,
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "go" and there is an empty object phrase.*)
val parse : string -> command