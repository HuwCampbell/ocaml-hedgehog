type t = Journal of string Seq.t

let singleton (msg : string) = Journal (Seq.cons msg Seq.empty)
let empty () = Journal Seq.empty
let append (Journal a) (Journal b) = Journal (Seq.append a b)

let eval (Journal entries : t) : string =
  Seq.fold_left (fun a b -> String.concat "\n" [ a; b ]) "" entries
