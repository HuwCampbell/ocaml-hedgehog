type 'a random = Random of (Seed.t -> Range.size -> 'a)

let unsafeRun (seed : Seed.t) (size : Range.size) (Random r : 'a random) : 'a =
  r seed size

let run (seed : Seed.t) (size : Range.size) (r : 'a random) : 'a =
  unsafeRun seed (max 1 size) r

let delay (f : unit -> 'a random) : 'a random =
  Random (fun seed size -> f () |> unsafeRun seed size)

let constant (x : 'a) : 'a random = Random (fun _ _ -> x)
let make (f : Seed.t -> Range.size -> 'a) : 'a random = Random f

let map (f : 'a -> 'b) (r : 'a random) : 'b random =
  Random (fun seed size -> r |> unsafeRun seed size |> f)

let bind (k : 'a -> 'b random) (r : 'a random) : 'b random =
  Random
    (fun seed size ->
      let seed1, seed2 = Seed.split seed in
      r |> unsafeRun seed1 size |> k |> unsafeRun seed2 size)

let int (range : int Range.t) : int random =
  Random
    (fun seed size ->
      let lo, hi = Range.bounds size range in
      let range = hi - lo in
      let x, _ = Seed.nextInt seed in
      (x mod range) + lo)
