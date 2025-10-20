type 'a t

val map : ('a -> 'b) -> 'a t -> 'b t
val bind : ('a -> 'b t) -> 'a t -> 'b t
val report : unit t -> Report.t
val render : unit t -> string
val forAllWith : ('a -> string) -> 'a Gen.t -> ('a -> 'b t) -> 'b t
val ofOutcome : 'a Outcome.t -> 'a t
val ofBool : bool -> unit t

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
