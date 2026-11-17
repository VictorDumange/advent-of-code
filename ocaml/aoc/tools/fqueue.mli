(** Persistent FIFO queue (two lists). *)

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val enqueue : 'a -> 'a t -> 'a t

(** Front element and the rest of the queue, or [None] if empty. *)
val dequeue : 'a t -> ('a * 'a t) option

module type ELT = sig
  type t
end

(** Same queue, specialised to one element type. *)
module Make (Elt : ELT) : sig
  type elt = Elt.t
  type t

  val empty : t
  val is_empty : t -> bool
  val enqueue : elt -> t -> t
  val dequeue : t -> (elt * t) option
end
