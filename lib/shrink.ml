let halves (n : int) : int Seq.t =
  let go x = if x = 0 then None else Some (x, x / 2) in
  Seq.unfold go n

let towards (destination : int) (x : int) : int Seq.t =
  if destination = x then Seq.empty
  else
    let diff = (x / 2) - (destination / 2) in
    halves diff |> Seq.map (fun y -> x - y) |> Seq.cons destination

let createTree (destination : int) (x : int) : int Tree.tree =
  let pairwise xs = Seq.zip xs (Seq.drop 1 xs) in
  let rec binarySearchTree ((destination : int), (x : int)) =
    let xs = towards destination x |> pairwise |> Seq.map binarySearchTree in
    Tree.create x xs
  in
  if destination = x then Tree.create x Seq.empty
  else binarySearchTree (destination, x) |> Tree.addChildValue destination
