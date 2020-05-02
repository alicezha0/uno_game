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

(** Raised when the card to be played is not in deck *)
exception CardNotInHand of card_name 

(** Raised when card to be played does not match last card played *)
exception MisMatch of card_name 

(** Raised when uno is called but it is not a valid uno, meaning that the gamer
    does not have one card. *)
exception Nouno of gamer

(** Raised when a gamer tries to play a card that does not match the power card 
    played in the last play *)
exception TallyIllegal of card_name 

(** [from_json j] is the initial game state formed from shuffling and dealing 
    the cards in the deck that [j] represents. The game rules require that the 
    int is 7, and that from_json is called. 
    Requires: [j] is a valid JSON deck representation. *)
val from_json : Yojson.Basic.t -> int -> t

(** [from_json_unshuffled j] is the game state formed from dealing the cards in 
    [j]. Formed only for testing purposes. Functionality and implementation
    same as from_json. *)
val from_json_unshuffled : Yojson.Basic.t -> int -> t 


(** [last_card_played t] is the name of the card that was played in the last 
    game state. *)
val last_card_played : t -> card_name

(** [last_card_played_color t] is the color of the card that was played in the last 
    game state. *)
val last_card_played_color : t -> string

(** [last_card_played_number t] is the number of the card that was played in the last 
    game state. *)
val last_card_played_number : t -> int

(** [hand t gamer] is a list of cards that the gamer has in hand. *)
val hand : t -> gamer -> card_name list

(** [hand_size t gamer] is the size of the gamer's deck. *)
val hand_size : t -> gamer -> int

(** [uno_state t gamer] is the uno_state of the gamer*)
val uno_state : t -> gamer -> bool 

(** [number_search t gamer card_name] is the number of the card in [gamer]'s 
    hand with name [card_name]*)
val number_search : t -> gamer -> card_name -> int 

(** [color_search t gamer card_name] is the color of the card in [gamer]'s 
    hand with name [card_name]*)
val color_search : t -> gamer -> card_name -> string 


(** [draw t gamer num] gives the next game state in which the gamer has drawn
    a number num cards. *)
val draw : t -> gamer -> int -> t

(** [play t gamer card_name] gives a new game state in which the card has been
    added to the discard pile and the gamer's hand has been adjusted 
    accordingly. 
    Raises: 
    1. (Mismatch of card_name) if the card to be played does not match the 
    the last_card_played 
    2. (CardNotInDeck of card_name) if the card to be played is not in the
    gamer's hand *)

val just_to_test: t -> card_name list

val play: t -> gamer -> card_name -> t

(** [uno_defensive t gamer] returns a new game state based on which gamer had 
    called uno for themself. If gamer has one card in hand, then their uno_state
    will be changed to true in the new game state. Otherwise, exception Nouno
    is thrown. This is called by command Uno of card_name.
    Raises: Nouno of gamer exception if the gamer has not called uno at the 
    appropriate time. *)
val uno_defensive : t -> gamer -> t

(** [uno_offensive t gamer1 gamer2] returns a new game state based on gamer1
    calling uno on gamer2. If this is a valid uno call, then gamer2 is forced to
    draw 4 cards. If this is an invalid uno call, then exception Nouno of gamer1
    is raised. This is called by command Uno2. 
    Assumption: gamer1 and gamer2 are not the same gamer. 
    Raises: Nouno of gamer1 exception if the gamer1 has not called uno at the 
    appropriate time. *)
val uno_offensive : t -> gamer -> gamer -> t

(**[ win_or_not gamer t] is true if [gamer] has no cards in their hand in game 
    state [t] and false otherwise. *)

val win_or_not : t -> gamer -> bool