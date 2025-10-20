type t

val from : int64 -> t
val random : unit -> t
val split : t -> t * t
val nextInt : t -> int * t
val nextInt64 : t -> int64 * t
val nextFloat : float -> float -> t -> float * t
