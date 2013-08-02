module Bitcoin.Signature

open Conv.Bytes
open Conv.Bitcoin
open Crypto
open Ecc
open Ecc.Curves

module funcs = 
  let messageMagic (message:byte[]) = 
    "\x18Bitcoin Signed Message:\n"B ++ [| byte message.Length |] ++ message

  let hashMagic = messageMagic >> sha256 >> sha256 >> toBigInt

  let firstOf items = items |> Seq.find Option.isSome |> Option.get

let verify addr msg sigb64  =
  let msghash = funcs.hashMagic msg
  let sigbytes = System.Convert.FromBase64String sigb64
  if sigbytes.Length <> 65 then Error "Bad signature encoding" else
  let r = sigbytes.[1..32] |> toBigInt
  let s = sigbytes.[33..] |> toBigInt
  let nV = int sigbytes.[0]
  if nV < 27 || nV >= 35 then Error "Bad message signature" else
  let compressed, recid = if nV >= 31 then (true, nV-4-27) else (false, nV-27)
  let x = r + (((bigint recid) / 2I) * secp256k1.Curve.n)
  let yIsOdd = (recid &&& 0x01) = 1
  let _, R = secp256k1.fromCompressed yIsOdd x
  let minus_e = secp256k1.Curve.n - msghash
  let inv_r = Arith.modInv r secp256k1.Curve.n
  let Q = secp256k1.multiply inv_r
            (secp256k1.add
              (secp256k1.multiply s R)
              (secp256k1.multiply minus_e secp256k1.Curve.G) )
  match secp256k1.ecdsa_verify Q msghash (r,s) with
  | Error err -> Error err
  | Valid p -> 
    let senderAddr = Pubkey.toAddress (compressed,Q)
    if addr <> senderAddr
    then Error "Invalid signature"
    else Valid senderAddr


let create ((cmp,privkey):PrivateKey) message =
  let msghash = (funcs.hashMagic message)
  let pubkey = secp256k1.getPubKey (cmp,privkey)
  let myaddress = Pubkey.toAddress pubkey
  let r,s = secp256k1.ecdsa_sign privkey (funcs.hashMagic message)
  funcs.firstOf (seq {
    for i in [0uy..4uy] ->
      let nV = 27uy + i + (if cmp then 4uy else 0uy)
      let sigb64 = [| nV |] ++ (uint256 r) ++ (uint256 s) |> System.Convert.ToBase64String
      match verify myaddress message sigb64  with
      | Error msg -> None
      | Valid address -> Some sigb64
    })
