type 'a tree = Node of 'a * 'a tree Seq.t

val outcome : 'a tree -> 'a
val shrinks : 'a tree -> 'a tree Seq.t
val fold : ('a -> 'b Seq.t -> 'b) -> 'a tree -> 'b
val map : ('a -> 'b) -> 'a tree -> 'b tree
val bind : ('a -> 'b tree) -> 'a tree -> 'b tree
val create : 'a -> 'a tree Seq.t -> 'a tree
val singleton : 'a -> 'a tree
val addChildValue : 'a -> 'a tree -> 'a tree
