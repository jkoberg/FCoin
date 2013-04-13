module Conv.Bitcoin

open EcDsa
open Crypto
open Conv.Bytes
open Conv.Base58

let toUncompressedPubKey (pubkey:PublicKey) = 
  match pubkey with
  | PointO -> failwith "Bad privkey"
  | Point(x, y) -> "\x04"B ++ uint256 x ++ uint256 y

let toPubKeyHex = toUncompressedPubKey >> Conv.Hex.fromBytes

let toAddressFormat = toUncompressedPubKey >> sha256 >> ripemd160 >> toBase58check 0uy

let toWalletImportFormat  = uint256 >> toBase58check 0x80uy

let fromWalletImportFormat wif : PrivateKey =
  match Base58.verify wif with
  | Some (0x80uy, payload) -> toBigInt payload
  | Some (v, _) -> failwith (sprintf "Valid version 0x%x base58check string, but not the version 0x80 private key expected." v)
  | None -> failwith (sprintf "Bad wallet import string: %s" wif)
