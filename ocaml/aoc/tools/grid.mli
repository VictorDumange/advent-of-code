type dir = N | E | S | W | NW | NE | SE | SW

val all_directions : dir list

type 'a t

val parse : string -> (char -> 'a) -> 'a t
val make : int -> int -> 'a -> 'a t

val rows : 'a t -> int
val cols : 'a t -> int

val get : int -> int -> 'a t -> 'a
val set : int -> int -> 'a -> 'a t -> unit

val direction_offset : dir -> int * int

(** [get_line i j d length grid] returns the [length] cells starting at
    [(i, j)] in direction [d], or [None] if it goes out of bounds. *)
val get_line : int -> int -> dir -> int -> 'a t -> 'a list option
