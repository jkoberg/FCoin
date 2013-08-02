namespace Ecc

open Conv.UnsignedBig

type Point = 
  | PointO
  | Point of  bigint * bigint


type CurveParams = 
  { a : bigint
    b : bigint
    p : bigint
    G : Point
    n : bigint
    h : bigint
    size : int
    }
  with
  static member secp256k1 = {
    size = 256
    p = fromHex "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFC2F"
    a = fromHex "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
    b = fromHex "00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000007"
    G = Point(fromHex "79BE667E F9DCBBAC 55A06295 CE870B07 029BFCDB 2DCE28D9 59F2815B 16F81798",
              fromHex "483ADA77 26A3C465 5DA4FBFC 0E1108A8 FD17B448 A6855419 9C47D08F FB10D4B8")
    n = fromHex "FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE BAAEDCE6 AF48A03B BFD25E8C D0364141"
    h = 1I
    }

type PrivateKey = bool * bigint

type PublicKey = bool * Point

type MaybeError<'a> =
  | Valid of 'a
  | Error of string

module Arith = 
  // redefine % to produce no negative values (for bigints)
  let (%) x m =
    if x < 0I
      then (x % m) + m
      else x % m

  // crappy non-tail-recursive implementation for now
  let rec egcd n1 n2 = 
    if n2 = 0I then
      (n1, 1I, 0I)
    else
      let quo, rem = bigint.DivRem(n1, n2)
      let d, s, t = egcd n2 rem
      (d, t, s - (quo * t))

  let modInv a m =
    match egcd a m with
    | x, y, _ when x = 1I -> y % m
    | x,y,r -> failwith "Invalid point in EcDsa curve reached"

  let modDiv a b p = (a * (modInv b p)) % p
