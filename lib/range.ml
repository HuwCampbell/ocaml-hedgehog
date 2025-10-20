(* Tests are parameterized by the `Size` of the randomly-generated data, *)
(* the meaning of which depends on the particular generator used. *)
type size = int
type 'a t = Range of 'a * (size -> 'a * 'a)

let origin (Range (origin, _) : 'a t) : 'a = origin
let singleton (o : 'a) : 'a t = Range (o, fun _ -> (o, o))

(*  Get the extents of a range, for a given size. *)
let bounds (sz : size) (Range (_, f) : 'a t) : 'a * 'a = f sz
let constant (a : int) (b : int) : int t = Range (a, fun _ -> (a, b))
