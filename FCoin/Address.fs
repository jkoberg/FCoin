module Address

open EcDsa
open Radix

let toAddressFormat privkey =
  match secp256k1.getPubKey privkey with
  | None -> failwith "Bad privkey"
  | Some PointO -> failwith "Bad privkey"
  | Some (Point(px,py)) ->
    let xbytes = BigEndian.fromBigInt px |> BigEndian.pad 32
    let ybytes = BigEndian.fromBigInt py |> BigEndian.pad 32
    let hashed = Array.concat [[|0x04uy|]; xbytes; ybytes] |> Digest.sha256 |> Digest.ripemd160
    Base58.toBase58check 0uy hashed
