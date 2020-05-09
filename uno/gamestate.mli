(**
   Representation of the state that the uno game is at currently.

   Contains information about 
   1. the gamers' hands
   2. the draw and discard piles
   3. the color_state of the game (the color that has to be matched against 
   for a legal play)
   4. the tally (for when +2 or +4 cards are played)

   Used by other modules for any changes to the game state, like when a legal 
   command is passed from either the User (the person playing the game) or the
   AI's.

   Also used to initialize the game before it can be played: shuffling the deck
   and dealing the cards to the gamers. 

   Throws exceptions when illegal attempts are made. 

*)

(** The abstract type of values representing the game state of uno. *)
type t 

(** The type of the uno card. *)
type card_name = string

(** The type of the gamer. User represents the person playing the game while
    player1, player2 and player3  represent the AI's . *)
type gamer = User | Player1 | Player2 | Player3

(** Raised when the card to be played is not in deck *)
exception CardNotInHand of card_name 

(** Raised when card to be played does not match last card played *)
exception MisMatch of card_name 

(** Raised when uno is called but it is not a valid uno, meaning that the gamer
    does not have one card. [gamer] is the gamer who called the invalid uno. *)
exception Nouno of gamer

(** Raised when a gamer tries to play a card that does not match the power card 
    played in the last play 
    Example: An attempt to play +2 after a +4 has been played in the last 
    round. *)
exception TallyIllegal 

(** Raised when someone tries to pass in an invalid gamer who is not currently
    playing the game. 
    Example: Player3 is passed in as an argument but there are only two AI's 
    playing the game. *)
exception InvalidGamer

(** [from_json j] is the initial game state formed from shuffling and dealing 
    the cards in the deck that [j] represents. The game rules require that the 
    first int is 7, and that from_json is called. The second int is the no. of 
    AI's in the game. 
    Requires: 1. [j] is a valid JSON deck representation.
              2. The second int is either 1, 2 or 3  *)
val from_json : Yojson.Basic.t -> int -> int -> t

(** [from_json_unshuffled j] is the game state formed from dealing the cards in 
    [j] without shuffling them first. Formed only for testing purposes.
    Functionality and implementation same as from_json. *)
val from_json_unshuffled : Yojson.Basic.t -> int -> int -> t 


(** [last_card_played t] is the name of the card that was played in the last 
    game state. *)
val last_card_played : t -> card_name

(** [last_card_played_color t] is the color of the card that was played in the 
    last game state. *)
val last_card_played_color : t -> string

(** [last_card_played_number t] is the number of the card that was played in the
    last game state. *)
val last_card_played_number : t -> int

(** [color_state t] is the color that has to be matched when a gamer plays a 
    card if that card is not a wild card at gamestate [t]*)
val color_state : t -> string

(** [current_tally_num t] is the number of the tally in the current gamestate *)
val current_tally_num : t -> int 

(** [current_tally_gamer t] is the gamer that the current tally is against *)
val current_tally_gamer: t -> gamer

(** [hand t gamer] represents the list of cards that the [gamer] has in 
    their hand. *)
val hand : t -> gamer -> card_name list

(** [hand_size t gamer] is the size of the [gamer]'s deck. *)
val hand_size : t -> gamer -> int

(** [uno_state t gamer] is a boolean that represents whether [gamer] has 
    called uno for themselves, and currently has only 1 card in their deck. *)
val uno_state : t -> gamer -> bool 

(** [number_search t gamer card_name] is the number of the card in [gamer]'s 
    hand with name [card_name]*)
val number_search : t -> gamer -> card_name -> int 

(** [color_search t gamer card_name] is the color of the card in [gamer]'s 
    hand with name [card_name]*)
val color_search : t -> gamer -> card_name -> string 

(** [draw t gamer num] is the next game state in which the gamer has drawn
    [num] no. of cards. It also adjusts [gamer]'s uno-state to false if it 
    was previously true. 
    Assumes: num is a non-zero int. 
*)
val draw : t -> gamer -> int -> t

(** [just_to_test t] is the list of cards in the draw pile in gamestate [t]. 
    This function is only used in test.ml for debugging purposes. *)
val just_to_test: t -> card_name list

(** [play t gamer gamer_n card_name col_str] gives a new game state in which the 
    card of [card_name] has been added to the discard pile and the [gamer]'s 
    hand has been adjusted accordingly. If a +2 or Wild +4 is played, then tally
    is adjusted against [gamer_n]. When a Wild card is played, the color state 
    of the game state is adjusted according to [col_str] which is [gamer]'s pick
    of the next color.
    Raises: 
    1. (Mismatch of card_name) if the card to be played does not match the 
    the last_card_played (according to the game rules)
    2. (CardNotInDeck of card_name) if the card to be played is not in the
    gamer's hand 
    3. (TallyIllegal) if the tally is not 0 and the card to be played does not 
    match the last action card played *)
val play: t -> gamer -> gamer -> card_name -> string -> t

(** [uno_defensive t gamer] is the new game state where [gamer] has 
    called uno for themself. If [gamer]'s deck has one card, then their 
    uno_state will be changed to true. 
    Raises: (Nouno of [gamer]) if the uno call is invalid (deck has more than 
    1 card) *)
val uno_defensive : t -> gamer -> t

(** [uno_offensive t gamer1 gamer2] is the new game state where [gamer1] has
    called uno on [gamer2]. If this is a valid uno call: [gamer2]'s uno_state is
    false and they have only 1 card in their deck, then [gamer2] is forced to
    draw 4 cards.
    Assumption: gamer1 and gamer2 are not the same gamer. 
    Raises: Nouno of [gamer1] exception if the gamer1 has not called uno at the 
    appropriate time. *)
val uno_offensive : t -> gamer -> gamer -> t

(**[ win_or_not gamer t] is true if [gamer] has no cards in their hand in game 
    state [t] and has called uno previously and false otherwise. *)
val win_or_not : t -> gamer -> bool