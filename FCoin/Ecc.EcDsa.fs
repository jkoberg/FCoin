namespace Ecc

open Arith

type EcDsa(c:CurveParams) = 
  inherit Ecc.Math(c)
  member this.ecdsa_sign privkey hash = 
    let z = hash
    let (_,k), (_,kG), r = this.newKeypair false
    let s = ((modInv k c.n) * ((z + ((r * privkey) % c.n)) % c.n) ) % c.n
    if s = 0I then failwith "Signature math failed. Probably a bug in this software." else
    (r, s)
        
  member this.ecdsa_verify pubkey hash (r,s) =
    let ( *~ ) = this.multiply
    let ( +~ ) = this.add
    let outOfRange i = i < 1I || i > c.n
    if outOfRange r || outOfRange s then Error "Invalid signature" else 
    match pubkey with
    | PointO -> Error "invalid pubkey"
    | pubkey when not (this.onCurve pubkey) -> Error "invalid pubkey"
    | pubkey when (c.n *~ pubkey) <> PointO -> Error "invalid pubkey"
    | pubkey ->
      let z = hash
      let w = modInv s c.n
      let u1 = (z * w) % c.n
      let u2 = (r * w) % c.n
      match (u1 *~ c.G) +~ (u2 *~ pubkey) with
      | PointO -> Error "Signature didn't match"
      | Point(x1, _) when r <> (x1 % c.n) -> Error "Signature didn't match"
      | _ as p -> Valid p


module Curves =
  open Conv.UnsignedBig

  let secp256k1 = EcDsa CurveParams.secp256k1
