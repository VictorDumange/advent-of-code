module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

(** Persistent min-priority queue (leftist heap). Duplicates are allowed. *)
module Make (Elt : ORDERED) : sig
  type elt = Elt.t
  type t

  val empty : t
  val is_empty : t -> bool

  val insert : elt -> t -> t

  (** Smallest element, or [None] if the queue is empty. *)
  val find_min : t -> elt option

  (** Queue without its smallest element (unchanged if empty). *)
  val remove_min : t -> t

  (** Smallest element and the rest of the queue. *)
  val pop_min : t -> (elt * t) option
end
