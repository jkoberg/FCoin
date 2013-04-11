module Conv.Bitcoin

open EcDsa.Arith
open EcDsa

open Conv.Bytes

let toAddressFormat (privkey:PrivateKey) =
  match secp256k1.getPubKey privkey with
  | PointO -> failwith "Bad privkey"
  | Point(px,py) ->
    let xbytes = fromBigInt px |> pad 32
    let ybytes = fromBigInt py |> pad 32
    let bytes = Array.concat [[|Magic.EcDsaUncompressedPubKey|]; xbytes; ybytes]
    bytes |> Digest.sha256 |> Digest.ripemd160 |> Base58.toBase58check Magic.BitcoinAddressVersion


let toWalletImportFormat (privkey:PrivateKey) =
  let keybytes = fromBigInt privkey |> pad 32
  keybytes |> Base58.toBase58check Magic.BitcoinPrivkeyVersion

let fromWalletImportFormat wif : PrivateKey =
  match Base58.verifyBase58check wif with
  | None -> failwith "Bad import string"
  | Some (version, payload) -> toBigInt payload