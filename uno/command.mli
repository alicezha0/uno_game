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
  | Commands

(** Legal2 is for when main calls rules or commands, which returns a string. *)
type result =
  | Illegal of string
  | Legal of Gamestate.t
  | Legal2 of string

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

(** [draw t g n] is Legal of the new game state in which the gamer [g] has 
    drawn [n] new card(s). *)
val draw : Gamestate.t -> Gamestate.gamer -> int -> result

(** [play t g phr] is Legal of the new game state or Illegal with an 
    error message for the gamer [g] who played the card [phr]. *)
val play : Gamestate.t -> Gamestate.gamer -> card_phrase -> result

(** [uno t g phr] returns Legal of the new game state or Illegal of an error
    message for the gamer [g] who played their second to last card [phr]. *)
val uno : Gamestate.t -> Gamestate.gamer -> card_phrase -> result

(** [uno2 t g1 g2] returns Legal of the new game state or Illegal of an error
    message from gamer 1 [g1] trying to call uno on gamer 2 [g2]. *)
val uno2 : Gamestate.t -> Gamestate.gamer -> Gamestate.gamer -> result

(** [rules] is the result Legal2 of the rules parsed from the rules.json file. 
*)
val rules : result

(** [commands] is the result Legal2 of the commands parsed from the 
    commands.json file. *)
val commands : result