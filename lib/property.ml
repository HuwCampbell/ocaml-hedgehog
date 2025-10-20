type 'a t = Property of (Journal.t * 'a Outcome.t) Gen.t

let unProp (Property a) = a

let counterexample (msg : string) : unit t =
  Property (Gen.constant (Journal.singleton msg, Outcome.Success ()))

let map (f : 'a -> 'b) (Property x : 'a t) : 'b t =
  let g (j, outcome) = (j, Outcome.map f outcome) in
  Property (Gen.map g x)

let bind (k : 'a -> 'b t) (Property m : 'a t) : 'b t =
  Property
    (Gen.bind
       (fun (j1, outcome) ->
         match outcome with
         | Outcome.Failure -> Gen.constant (j1, Outcome.Failure)
         | Outcome.Discard -> Gen.constant (j1, Outcome.Discard)
         | Outcome.Success x ->
             let (Property gka) = k x in
             Gen.map (fun (j2, ka) -> (Journal.append j1 j2, ka)) gka)
       m)

let set (a : 'a) : 'b t -> 'a t = map (fun _ -> a)

let both (a : 'a t) (b : 'b t) : ('a * 'b) t =
  bind (fun a0 -> map (fun b0 -> (a0, b0)) b) a

let ofOutcome (x : 'a Outcome.t) : 'a t =
  Property (Gen.constant (Journal.empty (), x))

let ofBool (x : bool) : unit t =
  ofOutcome (if x then Outcome.Success () else Outcome.Failure)

let splitAndRun (Property p : unit t) (seed : Seed.t) =
  let seed1, seed2 = Seed.split seed in
  let result = p |> Gen.toRandom |> Random.run seed1 100 in
  (result, seed2)

let shrinkInput (shrinkLimit : int option) =
  let rec loop (nshrinks : int) (t : (Journal.t * 'a Outcome.t) Tree.tree) =
    let reportRoot () = Report.Failed (nshrinks, fst (Tree.outcome t)) in
    match shrinkLimit with
    | Some shrinkLimit' when nshrinks >= shrinkLimit' -> reportRoot ()
    | _ -> (
        match
          Tree.shrinks t
          |> Seq.find (fun tt -> Outcome.isFailure (Tree.outcome tt |> snd))
        with
        | None -> reportRoot ()
        | Some tree -> loop (nshrinks + 1) tree)
  in
  loop 0

let reportWith (seed : Seed.t) (p : unit t) : Report.t =
  let rec loop seed tests discards : Report.t =
    if tests = 100 then { tests; discards; status = Report.OK }
    else if discards >= 100 then { tests; discards; status = Report.GaveUp }
    else
      let result, seed2 = splitAndRun p seed in
      let _, topResult = Tree.outcome result in
      match topResult with
      | Failure ->
          { tests = tests + 1; discards; status = shrinkInput None result }
      | Success () -> loop seed2 (tests + 1) discards
      | Discard -> loop seed2 tests (discards + 1)
  in
  loop seed 0 0

let report (p : unit t) : Report.t = reportWith (Seed.random ()) p
let render (p : unit t) : string = report p |> Report.render

let forAllWith (printer : 'a -> string) (gen : 'a Gen.t) (k : 'a -> 'b t) : 'b t
    =
  let prepend (x : 'a) =
    counterexample (printer x) |> set x |> bind k |> unProp
  in
  Property (gen |> Gen.bind prepend)

module type Applicative_syntax = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module Applicative_syntax : Applicative_syntax with type 'a t := 'a t = struct
  let ( let+ ) = fun a f -> map f a
  let ( and+ ) = both
end

module type Monad_syntax = sig
  include Applicative_syntax

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

module Monad_syntax : Monad_syntax with type 'a t := 'a t = struct
  include Applicative_syntax

  let ( let* ) = fun a f -> bind f a
  let ( and* ) = both
end
