type status = Failed of int * Journal.t | GaveUp | OK
type t = { tests : int; discards : int; status : status }

exception Falsifiable of string

let renderTests : int -> string = function
  | 1 -> "1 test"
  | n -> Printf.sprintf "%d tests" n

let renderDiscards : int -> string = function
  | 1 -> "1 discard"
  | n -> Printf.sprintf "%d discards" n

let renderAndDiscards : int -> string = function
  | 0 -> ""
  | 1 -> " and 1 discard"
  | n -> Printf.sprintf " and %d discards" n

let renderAndShrinks : int -> string = function
  | 0 -> ""
  | 1 -> " and 1 shrink"
  | n -> Printf.sprintf " and %d shrinks" n

let render (report : t) : string =
  match report.status with
  | OK -> Printf.sprintf "+++ OK, passed after %i tests" report.tests
  | GaveUp ->
      Printf.sprintf "*** Gave up after %s, passed %s."
        (renderDiscards report.discards)
        (renderTests report.tests)
  | Failed (shrinks, journal) ->
      Printf.sprintf "*** Failed! Falsifiable (after %s%s%s):\n%s"
        (renderTests report.tests) (renderAndShrinks shrinks)
        (renderAndDiscards report.discards)
        (Journal.eval journal)

let tryRaise (report : t) : unit =
  match report.status with
  | OK -> ()
  | GaveUp | Failed _ -> raise (Falsifiable (render report))
