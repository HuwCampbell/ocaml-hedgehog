type size = int
type 'a t

val origin : 'a t -> 'a
val singleton : 'a -> 'a t
val bounds : size -> 'a t -> 'a * 'a
val constant : int -> int -> int t
