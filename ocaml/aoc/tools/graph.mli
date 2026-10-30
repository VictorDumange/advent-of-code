module type NODE = sig
  type t
  val compare : t -> t -> int
end

module Make (Node : NODE) : sig
  type node = Node.t
  type t

  val make : unit -> t

  val add_node : node -> t -> t
  val add_edge : node -> node -> t -> t

  val get_neighbors : node -> t -> node list

  val nodes : t -> node list

  val topological_sort : t -> node list
end