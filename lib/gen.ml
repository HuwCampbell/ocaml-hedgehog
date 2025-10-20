type 'a t = Gen of 'a Tree.tree Random.random

let toRandom (Gen g : 'x t) : 'x Tree.tree Random.random = g

let mapRandom (f : 'a Tree.tree Random.random -> 'b Tree.tree Random.random)
    (Gen g : 'a t) : 'b t =
  Gen (f g)

let mapTree (f : 'a Tree.tree -> 't Tree.tree) (g : 'a t) : 'b t =
  mapRandom (Random.map f) g

let map (f : 'a -> 'b) (g : 'a t) : 'b t = mapTree (Tree.map f) g

let bindRandom (k : 'a -> 'b Tree.tree Random.random)
    (m : 'a Tree.tree Random.random) : 'b Tree.tree Random.random =
  Random.make (fun seed0 size ->
      let seed1, seed2 = Seed.split seed0 in
      Tree.bind (fun a -> Random.run seed2 size (k a)) (Random.run seed1 size m))

let bind (k : 'a -> 'b t) (g : 'a t) : 'b t =
  Gen (bindRandom (fun a -> toRandom (k a)) (toRandom g))

let both (a : 'a t) (b : 'b t) : ('a * 'b) t =
  bind (fun a0 -> map (fun b0 -> (a0, b0)) b) a

let int (range : 'a Range.t) : int t =
  Gen (Random.int range |> Random.map (Shrink.createTree (Range.origin range)))

let constant (a : 'a) : 'a t = Gen (Random.constant (Tree.singleton a))

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
