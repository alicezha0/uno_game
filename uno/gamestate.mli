(**
   Representation of the game data. 

   -- insert later --


*)

(** The abstract type of values representing the game state of uno. *)
type t 

(** The type of the uno card. *)
type card_name = string

(** The type of the gamer. User represents the person playing the game while
    player represents the AI. *)
type gamer = User | Player

(** Raised when an unknown card is encountered. *)
exception UnknownCard of card_name

(** Raised when the card to be played is not in deck *)
exception CardNotInDeck of card_name 

(** Raised when card to be played does not match last card played *)
exception MisMatch of card_name 

(** Raised when uno is called but it is not a valid uno, meaning that the gamer
    does not have one card. *)
exception Nouno of gamer

(** [from_json j] is the initial game state formed from arranging the cards in 
    the deck that [j] represents.
    Requires: [j] is a valid JSON deck representation. *)
val from_json : Yojson.Basic.t -> t

(** [last_card_played t] is the name of the card that was played in the last 
    game state. *)
val last_card_played : t -> card_name

(** [hand t gamer] is a list of cards that the gamer has in hand. *)
val hand : t -> gamer -> card_name list

(** [hand_size t gamer] is the size of the gamer's deck. *)
val hand_size : t -> gamer -> int

(** [draw t gamer num] gives the next game state in which the gamer has drawn
    a number num cards. *)
val draw : t -> gamer -> int -> t

(** [play t gamer card_name] gives a new game state in which the card has been
    added to the discard pile and the gamer's hand has been adjusted 
    accordingly. *)
val play: t -> gamer -> card_name -> t

(** [uno_defensive t gamer] returns a new game state based on which gamer had 
    called uno for themself. If gamer has one card in hand, then their uno_state
    has been changed to true and returns the new game state. Otherwise, throw
    exception Nouno. This is called by command Uno of card_name.
    Raises: Nouno of gamer exception if the gamer has not called uno at the 
    appropriate time. *)
val uno_defensive : t -> gamer -> t

(** [uno_offensive t gamer1 gamer2] returns a new game state based on gamer1
    calling uno on gamer2. If this is a valid uno call, then gamer2 is forced to
    draw 4 cards. If this is an invalid uno call, then exception Nouno of gamer1
    is raised. This is called by command Uno2. 
    Raises: Nouno of gamer1 exception if the gamer1 has not called uno at the 
    appropriate time. *)
val uno_offensive : t -> gamer -> gamer -> t

(**[ win_or_not gamer t] is true if [gamer] has no cards in their hand in game 
    state [t] and false otherwise. *)

val win_or_not : t -> gamer -> bool