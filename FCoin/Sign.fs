module Sign

open Conv.Bytes
open Crypto
open EcDsa
open Conv.Bitcoin

let messageMagic (message:byte[]) = 
  "\x18Bitcoin Signed Message:\n"B ++ [| byte message.Length |] ++ message

let hashMagic = messageMagic >> sha256 >> sha256 >> toBigInt

let verify_message addr msg sigb64  =
  let msghash = hashMagic msg
  let sigbytes = System.Convert.FromBase64String sigb64
  if sigbytes.Length <> 65 then Error "Bad signature encoding" else
  let r = sigbytes.[1..32] |> toBigInt
  let s = sigbytes.[33..] |> toBigInt
  let nV = int sigbytes.[0]
  if nV < 27 || nV >= 35 then Error "Bad message signature" else
  let compressed, recid = if nV >= 31 then (true, nV-4-27) else (false, nV-27)
  let x = r + (((bigint recid) / 2I) * secp256k1.n)
  let yIsOdd = (recid &&& 0x01) = 1
  let _, R = secp256k1.fromCompressed yIsOdd x
  let minus_e = secp256k1.n - msghash
  let inv_r = modInv r secp256k1.n
  let Q = secp256k1.multiply inv_r (
            secp256k1.add 
              (secp256k1.multiply s R) 
              (secp256k1.multiply minus_e secp256k1.G)
            )
  match secp256k1.verify Q msghash (r,s) with
  | Error err -> Error err
  | Valid p -> 
    let senderAddr = Pubkey.toAddress (compressed,Q)
    if addr <> senderAddr
    then Error "Invalid signature"
    else Valid senderAddr

let firstOf items = items |> Seq.find Option.isSome |> Option.get

let sign_message ((cmp,privkey):PrivateKey) compressed message =
  let pubkey = secp256k1.getPubKey (cmp,privkey)
  let myaddress = Pubkey.toAddress pubkey
  let msghash = (hashMagic message)
  let r,s = secp256k1.sign privkey (hashMagic message)
  let sigbytes = (uint256 r) ++ (uint256 s)
  let attempts = seq {
    for i in [0uy..4uy] ->
      let nV = 27uy + i + (if compressed then 4uy else 0uy)
      let sigb64 = [| nV |] ++ sigbytes |> System.Convert.ToBase64String
      match verify_message myaddress message sigb64  with
      | Error msg -> None
      | Valid address -> Some sigb64
      }
  firstOf attempts
