type 'a tree = Node of 'a * 'a tree Seq.t

module Node = struct
  let mapChildren (f : 'a tree Seq.t -> 'a tree Seq.t) (Node (x, xs) : 'a tree)
      : 'a tree =
    Node (x, f xs)
end

(* The generated outcome. *)
let outcome (Node (x, _) : 'a tree) : 'a = x

(* All the possible shrinks of this outcome. This should be ordered
smallest to largest as if property still fails with the first shrink in
the list then we will commit to that path and none of the others will
be tried (i.e. there is no backtracking). *)
let shrinks (Node (_, xs) : 'a tree) : 'a tree Seq.t = xs

let rec fold (f : 'a -> 'b Seq.t -> 'b) (Node (x, xs) : 'a tree) : 'b =
  f x (Seq.map (fold f) xs)

let map (f : 'a -> 'b) (tree : 'a tree) : 'b tree =
  tree |> fold (fun a stb -> Node (f a, stb))

let bind (f : 'a -> 'b tree) (tree : 'a tree) : 'b tree =
  tree |> fold (fun a stb -> a |> f |> Node.mapChildren (Seq.append stb))

let create (a : 'a) (children : 'a tree Seq.t) = Node (a, children)
let singleton (x : 'a) : 'a tree = Node (x, Seq.empty)

let addChild (child : 'a tree) (parent : 'a tree) : 'a tree =
  let (Node (x, xs)) = parent in
  Node (x, Seq.cons child xs)

let addChildValue (a : 'a) (tree : 'a tree) : 'a tree =
  tree |> addChild (singleton a)
