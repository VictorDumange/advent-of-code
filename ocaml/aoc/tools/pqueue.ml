module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module Make (Elt : ORDERED) = struct
  type elt = Elt.t

  (* rank = length of the rightmost path, kept shortest on the right *)
  type t =
    | Leaf
    | Node of int * elt * t * t

  let empty = Leaf

  let is_empty = function
    | Leaf -> true
    | Node _ -> false

  let rank = function
    | Leaf -> 0
    | Node (r, _, _, _) -> r

  let make_node x a b =
    if rank a >= rank b then Node (rank b + 1, x, a, b)
    else Node (rank a + 1, x, b, a)

  let rec merge h1 h2 =
    match h1, h2 with
    | Leaf, h | h, Leaf -> h
    | Node (_, x, a1, b1), Node (_, y, _, _) ->
        if Elt.compare x y <= 0 then make_node x a1 (merge b1 h2)
        else merge h2 h1

  let insert x heap =
    merge (Node (1, x, Leaf, Leaf)) heap

  let find_min = function
    | Leaf -> None
    | Node (_, x, _, _) -> Some x

  let remove_min = function
    | Leaf -> Leaf
    | Node (_, _, a, b) -> merge a b

  let pop_min = function
    | Leaf -> None
    | Node (_, x, a, b) -> Some (x, merge a b)
end
