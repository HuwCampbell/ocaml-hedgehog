type 'a t = Failure | Discard | Success of 'a

let cata (outcome : 'a t) (failure : unit -> 'b) (discard : unit -> 'b)
    (success : 'a -> 'b) : 'b =
  match outcome with
  | Failure -> failure ()
  | Discard -> discard ()
  | Success x -> success x

let map (f : 'a -> 'b) (result : 'a t) : 'b t =
  cata result (fun _ -> Failure) (fun _ -> Discard) (fun a -> Success (f a))

let isFailure (result : 'a t) : bool =
  cata result (fun _ -> true) (fun _ -> false) (fun _ -> false)
