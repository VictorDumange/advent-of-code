type 'a t = {
  front : 'a list;
  back : 'a list;
}

let empty = { front = []; back = [] }

let is_empty q =
  q.front = [] && q.back = []

let enqueue x q =
  { q with back = x :: q.back }

let normalize q =
  match q.front with
  | [] -> { front = List.rev q.back; back = [] }
  | _ -> q

let dequeue q =
  let q = normalize q in
  match q.front with
  | [] -> None
  | x :: xs -> Some (x, { q with front = xs })

module type ELT = sig
  type t
end

module Make (Elt : ELT) = struct
  type elt = Elt.t
  type nonrec t = elt t

  let empty = empty
  let is_empty = is_empty
  let enqueue = enqueue
  let dequeue = dequeue
end
