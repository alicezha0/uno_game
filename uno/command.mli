(**
   Command handles the connection between User input and the Gamestate module. 
   It parses string inputs from the gamer and gives a type command or color. 
   This also contains the functions for each of the type command, giving a type
   result, which is the new Gamestate or a helpful error message.
*)


(** The type [card_phrase] represents the name of the card that the gamer 
    inputs. For example, if the gamer command is 'Play Red 2', then 
    [card_phrase] is 'Red 2'. *)
type card_phrase = string

(** The type [command] represents a gamer command that is decomposed
    into a verb. Some commands have a [card_phrase] or gamer 
    attached to the command type. *)
type command = 
  | Draw
  | Play of card_phrase
  | Uno of card_phrase
  | Uno2 of Gamestate.gamer
  | Rules
  | Commands
  | Quit

(** The type [result] represents whether the gamer has called for a legal
    command. If so, a new Gamestate is given. Otherwise, a helpful error message
    will be given. *)
type result =
  | Illegal of string
  | Legal of Gamestate.t

(** The type [color] is the color that the gamer chooses to be played next if
    a wild card was played. *)
type color =
  | Red
  | Yellow
  | Green
  | Blue
  | Any

(** Raised when an empty command or color has been attempted to be parsed. *)
exception Empty

(** Raised when a malformed command or color is encountered. *)
exception Malformed

(** [parse str] parses a gamer's input into a [command], as follows. The first
    word of [str] becomes the verb. The rest of the words, if any, become the 
    card_phrase or player.
    Examples: 
    - [parse "play Red 2"] is [Play "red 2"]
    - [parse "rules"] is [Rules]. 
    - [parse "uno2 player1"]

    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [Malformed] if the command is malformed. *)
val parse : string -> command

(** [parse_color str] parses a gamer's input into a [color]. 
    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [Malformed] if the color is malformed. *)
val parse_color : string -> color

(** [draw gs gamer num] is [result], which is always Legal of the new game 
    state in which the [gamer] has drawn [num] new card(s). *)
val draw : Gamestate.t -> Gamestate.gamer -> int -> result

(** [play gs gamer next_gamer phr clr] is a [result], described as follows.
    Legal of the new Gamestate or Illegal with an error message for 
    the [gamer] who played the card [phr] during the current Gamestate [gs]. 
    [clr] is the [color] of the next card to be played. *)
val play : Gamestate.t -> Gamestate.gamer -> Gamestate.gamer ->
  card_phrase -> color -> result

(** [uno gs gamer next_gamer phr clr] is a [result], described as follows. 
    Legal of the new Gamestate or Illegal with an error message for the [gamer] 
    who did not have a valid uno with card [phr]. [clr] is the [color] of the 
    next card to be played. *)
val uno : Gamestate.t -> Gamestate.gamer -> Gamestate.gamer ->
  card_phrase -> color -> result

(** [uno2 gs gamer1 gamer2] is a [result], described as follows. 
    Legal of the new Gamestate or Illegal with an error message for the [gamer1]
    trying to call uno on [gamer2] who either does not have uno or has called it
    for themselves already. *)
val uno2 : Gamestate.t -> Gamestate.gamer -> Gamestate.gamer -> result

(** [rules] is the rules parsed from the rules.json file. *)
val rules : string

(** [commands] is the commands parsed from the commands.json file. *)
val commands : string