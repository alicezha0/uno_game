open Yojson.Basic.Util

(** [shuffle t] returns a new game state with the cards in the discard pile
    shuffled and . this is called by need_shuffle *)

val shuffle : t -> t

(** [need_shuffle t] returns whether the discard pile needs shuffling. this will
    be called from draw. *)
val need_shuffle : t -> bool