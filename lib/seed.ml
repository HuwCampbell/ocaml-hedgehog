(* Splittable random number generator. *)
type t = {
  value : int64;
  (* Must be an odd number. *)
  gamma : int64;
}

let next (seed : t) : int64 * t =
  let g = seed.gamma in
  let v = Int64.add seed.value g in
  (v, { value = v; gamma = g })

let mix64 (x : int64) : int64 =
  let y =
    Int64.mul (Int64.logxor x (Int64.shift_right x 33)) 0xff51afd7ed558ccdL
  in
  let z =
    Int64.mul (Int64.logxor y (Int64.shift_right y 33)) 0xc4ceb9fe1a85ec53L
  in
  Int64.logxor z (Int64.shift_right z 33)

let mix64variant13 (x : int64) : int64 =
  let y =
    Int64.mul (Int64.logxor x (Int64.shift_right x 30)) 0xbf58476d1ce4e5b9L
  in
  let z =
    Int64.mul (Int64.logxor y (Int64.shift_right y 27)) 0x94d049bb133111ebL
  in
  Int64.logxor z (Int64.shift_right z 31)

let mixGamma (x : int64) : int64 =
  let y = Int64.logor (mix64variant13 x) 1L in
  y

let mixSeed (value : int64) (gamma : int64) : t =
  { value = mix64 value; gamma = mixGamma gamma }

(* Generates a random 'int64'. *)
let nextInt64 (seed : t) : int64 * t =
  let v, s = next seed in
  (mix64 v, s)

(* Generates a random 'int'. *)
let nextInt (seed : t) : int * t =
  let v, s = nextInt64 seed in
  (Int64.to_int v, s)

let rec nextFloat (lo : float) (hi : float) (seed : t) : float * t =
  if lo > hi then nextFloat hi lo seed
  else
    let x, seed' = seed |> next in
    let scaledX =
      (0.5 *. lo) +. (0.5 *. hi)
      +. ((0.5 *. hi) -. (0.5 *. lo))
         /. (0.5 *. 4294967296.0) *. Int64.to_float x
    in
    (scaledX, seed')

(* Splits a 'Seed' in to two. *)
let split (seed : t) : t * t =
  let value, seed1 = next seed in
  let gamma, seed2 = next seed1 in
  (seed2, mixSeed value gamma)

let golden : int64 = 0x9e3779b97f4a7c15L
let from (x : int64) : t = mixSeed x (Int64.add x golden)
let random () : t = from (Int64.of_float (Unix.gettimeofday () *. 10000.0))
