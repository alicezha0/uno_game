(**
   This module parses the string input from the gamer and gives a type command.
   This also contains the functions for each of the type command, which will 
   give a type result and a new Gamestate or string or an error message.
*)

(* (** The type [card_phrase] represents name of the card that will be part of a 
    player command.
   - If the player command is ["Play Red 2"], then the card phrase is 
      ["Red 2"].

    A [card_phrase] is not permitted to be the empty list. *)
   type card_phrase = string *)
type card_phrase = string

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a card_phrase. *)
type command = 
  | Draw
  | Play of card_phrase
  | Uno of card_phrase
  | Uno2
  | Rules
  | Commands
  | Quit

(** Legal2 is for when main calls rules or commands, which returns a string. *)
type result =
  | Illegal of string
  | Legal of Gamestate.t

(** Raised when an empty command or card_phrase is parsed. *)
exception Empty

(** Raised when a malformed command or card_phrase is encountered. *)
exception Malformed

(** [parse str] parses a gamer's input into a [command], as follows. The first
    word of [str] becomes the verb. The rest of the words, if any, become the 
    card_phrase.
    Examples: 
    - [parse "play Red 2"] is [Play "red 2"]
    - [parse "rules"] is [Rules]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [Malformed] if the command is malformed. *)
val parse : string -> command

(** [draw gs gamer num] is Legal of the new game state in which the gamer
    [gamer] has drawn number [num] new card(s). *)
val draw : Gamestate.t -> Gamestate.gamer -> int -> result

(** [play gs gamer phr] is Legal of the new game state or Illegal with an 
    error message for the [gamer] who played the card [phr]. *)
val play : Gamestate.t -> Gamestate.gamer -> card_phrase -> result

(** [uno gs gamer phr] is Legal of the new game state or Illegal of an 
    error message for the [gamer] who did not have a valid uno with card 
    [phr]. *)
val uno : Gamestate.t -> Gamestate.gamer -> card_phrase -> result

(** [uno2 gs gamer1 gamer2] is Legal of the new game state or Illegal of 
    an error message for the gamer who called uno [gamer1] trying to call uno on 
    the other gamer [gamer2]. *)
val uno2 : Gamestate.t -> Gamestate.gamer -> Gamestate.gamer -> result

(** [rules] is the rules parsed from the rules.json file. *)
val rules : string

(** [commands] is the commands parsed from the commands.json file. *)
val commands : string