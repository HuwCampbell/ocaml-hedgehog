type 'a random

val run : Seed.t -> Range.size -> 'a random -> 'a
val delay : (unit -> 'a random) -> 'a random
val constant : 'a -> 'a random
val make : (Seed.t -> Range.size -> 'a) -> 'a random
val map : ('a -> 'b) -> 'a random -> 'b random
val bind : ('a -> 'b random) -> 'a random -> 'b random
val int : int Range.t -> int random
