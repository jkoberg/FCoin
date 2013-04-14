module Conv.Bitcoin

open EcDsa
open Crypto
open Conv.Bytes
open Conv.Base58

let toUncompressedPubKey (pubkey:PublicKey) = 
  match pubkey with
  | PointO -> "\x00"B
  | Point(x, y) -> "\x04"B ++ uint256 x ++ uint256 y

let toCompressedPubkey (pubkey:PublicKey) = 
  match pubkey with
  | PointO -> "\x00"B
  | Point(x,y) when y.IsEven -> "\x02"B ++ (uint256 x)
  | Point(x,y) -> "\x03"B ++ (uint256 x)

let encodePrivkey compressed (privkey:PrivateKey) = 
  let pubkey = secp256k1.getPubKey privkey
  if compressed then toCompressedPubkey pubkey
  else toUncompressedPubKey pubkey

let encodePubkey compressed (pubkey:PublicKey) = 
  if compressed then toCompressedPubkey pubkey
  else toUncompressedPubKey pubkey

let fromEncodedPubkey (encoded:byte[]) =
  match encoded.[0] with
  | 0x00uy -> PointO
  | 0x02uy -> secp256k1.fromCompressed false (toBigInt encoded.[1..]) 
  | 0x03uy -> secp256k1.fromCompressed true (toBigInt encoded.[1..])
  | 0x04uy -> Point(toBigInt encoded.[1..32], toBigInt encoded.[33..])
  | _ -> failwith "Unknown key prefix byte"


let toPubKeyHex = toUncompressedPubKey >> Conv.Hex.fromBytes

let toAddressFormat = toUncompressedPubKey >> sha256 >> ripemd160 >> toBase58check 0uy

let toAddress compressed = encodePubkey compressed >> sha256 >> ripemd160 >> toBase58check 0uy

let toWalletImportFormat  = uint256 >> toBase58check 0x80uy

let fromWalletImportFormat wif : PrivateKey =
  match Base58.verify wif with
  | Some (0x80uy, payload) -> toBigInt payload
  | Some (v, _) -> failwith (sprintf "Valid version 0x%x base58check string, but not the version 0x80 private key expected." v)
  | None -> failwith (sprintf "Bad wallet import string: %s" wif)

