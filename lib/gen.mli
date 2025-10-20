type 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
val bind : ('a -> 'b t) -> 'a t -> 'b t
val int : int Range.t -> int t
val constant : 'a -> 'a t
val toRandom : 'a t -> 'a Tree.tree Random.random

module type Applicative_syntax = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module type Monad_syntax = sig
  include Applicative_syntax

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end
