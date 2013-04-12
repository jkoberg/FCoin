module Conv.Bitcoin

open EcDsa
open Crypto
open Conv.Bytes
open Conv.Base58

let toUncompressedPubKey (pubkey:PublicKey) = 
  match pubkey with
  | PointO -> failwith "Bad privkey"
  | Point(px,py) -> "\x04"B ++ uint256 px ++ uint256 py

let toPubKeyHex = toUncompressedPubKey >> Conv.Hex.fromBytes

let toAddressFormat = toUncompressedPubKey >> sha256 >> ripemd160 >> toBase58check 0uy

let toWalletImportFormat  = uint256 >> toBase58check 0x80uy

let fromWalletImportFormat wif : PrivateKey =
  match Base58.verify wif with
  | Some (version, payload) -> toBigInt payload
  | None -> failwith (sprintf "Bad wallet import string: %s" wif)