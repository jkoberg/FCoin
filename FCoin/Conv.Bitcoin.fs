module Conv.Bitcoin

open EcDsa
open Crypto
open Conv.Bytes
open Conv.Base58

module Pubkey =
  let fromBytes (encoded:byte[]) : PublicKey =
    match encoded.[0] with
    | 0x00uy -> false, PointO
    | 0x02uy -> secp256k1.fromCompressed false (toBigInt encoded.[1..]) 
    | 0x03uy -> secp256k1.fromCompressed true (toBigInt encoded.[1..])
    | 0x04uy -> false, Point(toBigInt encoded.[1..32], toBigInt encoded.[33..])
    | _ -> failwith "Unknown key prefix byte"

  let toBytes (p:PublicKey) = 
    match p with
    | _, PointO -> "\x00"B
    | false, Point(x,y) -> "\x04"B ++ uint256 x ++ uint256 y
    | true, Point(x,y) when y.IsEven -> "\x02"B ++ (uint256 x)
    | true, Point(x,y) -> "\x03"B ++ (uint256 x)

  let toHex = toBytes >> toHex

  let toAddress = toBytes >> sha256 >> ripemd160 >> toBase58check 0uy

  let fromPrivate = secp256k1.getPubKey


module Privkey =
  let fromBytes (encoded:byte[]) : PrivateKey = 
    if encoded.Length = 32
      then (false, (encoded |> toBigInt))
      else 
        if encoded.[33] = 0x01uy
        then(true, (encoded.[..32] |> toBigInt))
        else failwith  "bad privkey encoding"

  let toBytes ((compressed,k):PrivateKey) = 
    let body = uint256 k
    if compressed then body ++ "\x01"B
    else body

  let toWalletImportFormat = toBytes >> toBase58check 0x80uy

  let toHex = toBytes >> toHex

  let toAddress = Pubkey.fromPrivate >> Pubkey.toAddress

  let fromWalletImportFormat wif : PrivateKey =
    match Base58.verify wif with
    | Some (0x80uy, payload) when payload.Length = 32 -> (false, toBigInt payload)
    | Some (0x80uy, payload) when payload.Length = 33 && payload.[33] = 0x01uy -> (true, toBigInt payload)
    | Some (v, _) -> failwith (sprintf "Valid version 0x%x base58check string, but not the version 0x80 private key expected." v)
    | None -> failwith (sprintf "Bad wallet import string: %s" wif)


  








